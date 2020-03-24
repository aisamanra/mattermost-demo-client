{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
-- | This module provides the login interface for Matterhorn.
--
-- * Overview
--
-- The interface provides a set of form fields for the user to use to
-- enter their server information and credentials. The user enters
-- this information and presses Enter, and then this module
-- attempts to connect to the server. The module's main function,
-- interactiveGetLoginSession, returns the result of that connection
-- attempt, if any.
--
-- * Details
--
-- The interactiveGetLoginSession function takes the Matterhorn
-- configuration's initial connection information as input. If the
-- configuration provided a complete set of values needed to make a
-- login attempt, this module goes ahead and immediately makes a login
-- attempt before even showing the user the login form. This case is
-- the case where the configuration provided all four values needed:
-- server host name, port, username, and password. When the interface
-- immediately makes a login attempt under these conditions, this is
-- referred to as an "initial" attempt in various docstrings below.
-- Otherwise, the user is prompted to fill out the form to enter any
-- missing values. On pressing Enter, a login attempt is made.
--
-- A status message about whether a connection is underway is shown in
-- both cases: in the case where the user has edited the credentials and
-- pressed Enter, and in the case where the original credentials
-- provided to interactiveGetLoginSession caused an initial connection
-- attempt.
--
-- The "initial" login case is special because in addition to not
-- showing the form, we want to ensure that the "connecting to..."
-- status message that is shown is shown long enough for the user to
-- see what is happening (rather than just flashing by in the case
-- of a fast server connection). For this usability reason, we have
-- a "startup timer" thread: the thread waits a specified number of
-- milliseconds (see below) and then notifies the interface that it has
-- timed out. If there is an initial connection attempt underway that
-- succeeds *before* the timer fires, we wait until the timer fires
-- before quitting the application. This ensures that the "connecting
-- to..." message stays on the screen long enough to not be jarring, and
-- to show the user what is happening. If the connection fails before
-- the timer fires, we just resume normal operation and show the login
-- form so the user can intervene.
module Login
  ( LoginAttempt(..)
  , interactiveGetLoginSession
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.BChan
import           Brick.Focus
import           Brick.Forms
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Edit
import           Control.Concurrent ( forkIO, threadDelay )
import           Control.Exception ( catch )
import           System.IO.Error ( catchIOError )
import qualified Data.Text as T
import           Data.Text (unpack)
import           Graphics.Vty hiding (mkVty)
import           Lens.Micro.Platform ( (.~), Lens', makeLenses )
import qualified System.IO.Error as Err

import           Network.Mattermost ( ConnectionData )
import           Network.Mattermost.Types ( Session(..), User, UserParam(..), Login(..), ConnectionPoolConfig(..)
                                          , initConnectionData, ConnectionType(..))
import           Network.Mattermost.Types.Internal ( Token(..) )
import           Network.Mattermost.Exceptions ( LoginFailureException(..) )
import           Network.Mattermost.Endpoints ( mmLogin, mmGetUser )

import           Markdown
import           Themes ( clientEmphAttr )
import           Types ( ConnectionInfo(..)
                       , ConnectionInfoUsernamePassword(..)
                       , cipPassword, cipUsername, cipHostname, cipPort
                       , citToken, citHostname, citPort, getHostname, getPort, AuthenticationException(..)
                       , LogManager, LogCategory(..), ioLogWithManager
                       )


-- | Resource names for the login interface.
data Name =
    Hostname
    | Port
    | Username
    | Password
    deriving (Ord, Eq, Show)

-- | The result of an authentication attempt.
data LoginAttempt =
    AttemptFailed AuthenticationException
    -- ^ The attempt failed with the corresponding error.
    | AttemptSucceeded ConnectionInfo ConnectionData Session User
    -- ^ The attempt succeeded.

-- | The state of the login interface: whether a login attempt is
-- currently in progress.
data LoginState =
    Idle
    -- ^ No login attempt is in progress.
    | Connecting Bool Text
    -- ^ A login attempt to the specified host is in progress. The
    -- boolean flag indicates whether this login was user initiated
    -- (False) or triggered immediately when starting the interface
    -- (True). This "initial" flag is used to determine whether the
    -- login form is shown while the connection attempt is underway.
    deriving (Eq)

-- | Requests that we can make to the login worker thead.
data LoginRequest =
    DoLogin Bool ConnectionInfo
    -- ^ Request a login using the specified connection information.
    -- The boolean flag is the "initial" flag value corresponding to the
    -- "Connecting" constructor flag of the "LoginState" type.

-- | The messages that the login worker thread can send to the user
-- interface event handler.
data LoginEvent =
    StartConnect Bool Text
    -- ^ A connection to the specified host has begun. The boolean
    -- value is whether this was an "initial" connection attempt (see
    -- LoginState).
    | LoginResult LoginAttempt
    -- ^ A login attempt finished with the specified result.
    | StartupTimeout
    -- ^ The startup timer thread fired.

-- | The login application state.
data State =
    State { _loginForm :: Form ConnectionInfoUsernamePassword LoginEvent Name
          , _lastAttempt :: Maybe LoginAttempt
          , _currentState :: LoginState
          , _reqChan :: BChan LoginRequest
          , _timeoutFired :: Bool
          }

makeLenses ''State

-- | The HTTP connection pool settings for the login worker thread.
poolCfg :: ConnectionPoolConfig
poolCfg = ConnectionPoolConfig { cpIdleConnTimeout = 60
                               , cpStripesCount = 1
                               , cpMaxConnCount = 5
                               }

-- | Run an IO action and convert various kinds of thrown exceptions
-- into a returned AuthenticationException.
convertLoginExceptions :: IO a -> IO (Either AuthenticationException a)
convertLoginExceptions act =
    (Right <$> act)
        `catch` (\e -> return $ Left $ ResolveError e)
        `catch` (\e -> return $ Left $ ConnectError e)
        `catchIOError` (\e -> return $ Left $ AuthIOError e)
        `catch` (\e -> return $ Left $ OtherAuthError e)

-- | The login worker thread.
loginWorker :: (ConnectionData -> ConnectionData)
            -- ^ The function used to set the logger on the
            -- ConnectionData that results from a successful login
            -- attempt.
            -> LogManager
            -- ^ The log manager used to do logging.
            -> ConnectionType
            -- ^ The kind of connection to make to the server.
            -> BChan LoginRequest
            -- ^ The channel on which we'll await requests.
            -> BChan LoginEvent
            -- ^ The channel to which we'll send login attempt results.
            -> IO ()
loginWorker setLogger logMgr connTy requestChan respChan = forever $ do
    req <- readBChan requestChan
    case req of
        DoLogin initial connInfo -> do
            let hostname = getHostname connInfo
            let port = getPort connInfo
            writeBChan respChan $ StartConnect initial $ hostname
            ioLogWithManager logMgr Nothing LogGeneral $ "Attempting authentication to " <> hostname

            cd <- fmap setLogger $ initConnectionData hostname (fromIntegral port) connTy poolCfg

            case connInfo of
                UsernamePassword cip -> do
                    let login = Login { username = cip^.cipUsername
                                      , password = cip^.cipPassword
                                      }
                    result <- convertLoginExceptions $ mmLogin cd login
                    case result of
                        Left e -> do
                            ioLogWithManager logMgr Nothing LogGeneral $
                                "Error authenticating to " <> hostname <> ": " <> (T.pack $ show e)
                            writeBChan respChan $ LoginResult $ AttemptFailed e
                        Right (Left e) -> do
                            ioLogWithManager logMgr Nothing LogGeneral $
                                "Error authenticating to " <> hostname <> ": " <> (T.pack $ show e)
                            writeBChan respChan $ LoginResult $ AttemptFailed $ LoginError e
                        Right (Right (sess, user)) -> do
                            ioLogWithManager logMgr Nothing LogGeneral $
                                "Authenticated successfully to " <> hostname <> " as " <> cip^.cipUsername
                            writeBChan respChan $ LoginResult $ AttemptSucceeded connInfo cd sess user
                Types.Token cit -> do
                    let sess = Session cd $ Network.Mattermost.Types.Internal.Token $ unpack $ cit^.citToken

                    user <- mmGetUser UserMe sess
                    writeBChan respChan $ LoginResult $ AttemptSucceeded connInfo cd sess user


-- | The amount of time that the startup timer thread will wait before
-- firing.
startupTimerMilliseconds :: Int
startupTimerMilliseconds = 750

-- | The startup timer thread.
startupTimer :: BChan LoginEvent -> IO ()
startupTimer respChan = do
    threadDelay $ startupTimerMilliseconds * 1000
    writeBChan respChan StartupTimeout

-- | The main function of this module: interactively present a login
-- interface, get the user's input, and attempt to log into the user's
-- specified mattermost server.
--
-- This always returns the final terminal state handle. If the user
-- makes no login attempt, this returns Nothing. Otherwise it returns
-- Just the result of the latest attempt.
interactiveGetLoginSession :: Vty
                           -- ^ The initial terminal state handle to use.
                           -> IO Vty
                           -- ^ An action to build a new state handle
                           -- if one is needed. (In practice this
                           -- never fires since the login app doesn't
                           -- use suspendAndResume, but we need it to
                           -- satisfy the Brick API.)
                           -> ConnectionType
                           -- ^ The kind of connection to make to the
                           -- mattermost server.
                           -> (ConnectionData -> ConnectionData)
                           -- ^ The function to set the logger on
                           -- connection handles.
                           -> LogManager
                           -- ^ The log manager used to do logging.
                           -> ConnectionInfo
                           -- ^ Initial connection info to use to
                           -- populate the login form. If the connection
                           -- info provided here is fully populated, an
                           -- initial connection attempt is made without
                           -- first getting the user to hit Enter.
                           -> IO (Maybe LoginAttempt, Vty)
interactiveGetLoginSession vty mkVty connTy setLogger logMgr initialConfig = do
    requestChan <- newBChan 10
    respChan <- newBChan 10

    void $ forkIO $ loginWorker setLogger logMgr connTy requestChan respChan
    void $ forkIO $ startupTimer respChan


    -- The TUI can only be used to make a login attempt using username and password
    -- Hence, if the config specified a token login, we convert the connection info
    -- to the username/password variant first.
    let tuiConfig = case initialConfig of
                         UsernamePassword up -> up
                         Types.Token t -> ConnectionInfoUsernamePassword
                                              { _cipHostname = t^.citHostname
                                              , _cipPort = t^.citPort
                                              , _cipUsername = ""
                                              , _cipPassword = ""
                                              }

    let initialState = mkState tuiConfig requestChan

    startState <- case (populatedConnectionInfo initialConfig) of
        True -> do
            writeBChan requestChan $ DoLogin True initialConfig
            return $ initialState & currentState .~ Connecting True (getHostname initialConfig)
        False -> do
            return initialState

    (finalSt, finalVty) <- customMainWithVty vty mkVty (Just respChan) app startState

    return (finalSt^.lastAttempt, finalVty)

-- | Is the specified ConnectionInfo sufficiently populated for us to
-- bother attempting to use it to connect?
populatedConnectionInfo :: ConnectionInfo -> Bool
populatedConnectionInfo ci =
    and [ not $ T.null $ getHostname ci
        , (getPort ci) > 0
        , case ci of
          UsernamePassword cip -> and [ not $ T.null $ cip^.cipUsername
                                      , not $ T.null $ cip^.cipPassword
                                      ]
          Types.Token cit -> not $ T.null $ cit^.citToken
        ]

-- | Make an initial login application state.
mkState :: ConnectionInfoUsernamePassword -> BChan LoginRequest -> State
mkState cInfo chan = state
    where
        state = State { _loginForm = form { formFocus = focusSetCurrent initialFocus (formFocus form)
                                          }
                      , _currentState = Idle
                      , _lastAttempt = Nothing
                      , _reqChan = chan
                      , _timeoutFired = False
                      }
        form = mkForm cInfo
        initialFocus = if | T.null (cInfo^.cipHostname) -> Hostname
                          | T.null (cInfo^.cipUsername) -> Username
                          | T.null (cInfo^.cipPassword) -> Password
                          | otherwise                  -> Hostname

app :: App State LoginEvent Name
app = App
  { appDraw         = credsDraw
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = onEvent
  , appStartEvent   = return
  , appAttrMap      = const colorTheme
  }

onEvent :: State -> BrickEvent Name LoginEvent -> EventM Name (Next State)
onEvent st (VtyEvent (EvKey KEsc [])) = do
    halt $ st & lastAttempt .~ Nothing
onEvent st (AppEvent (StartConnect initial host)) = do
    continue $ st & currentState .~ Connecting initial host
                  & lastAttempt .~ Nothing
onEvent st (AppEvent StartupTimeout) = do
    -- If the startup timer fired and we have already succeeded, halt.
    case st^.lastAttempt of
        Just (AttemptSucceeded {}) -> halt st
        _ -> continue $ st & timeoutFired .~ True
onEvent st (AppEvent (LoginResult attempt)) = do
    let st' = st & lastAttempt .~ Just attempt
                 & currentState .~ Idle

    case attempt of
        AttemptSucceeded {} -> do
            -- If the startup timer already fired, halt. Otherwise wait
            -- until that timer fires.
            case st^.timeoutFired of
                True -> halt st'
                False -> continue st'
        AttemptFailed {} -> continue st'

onEvent st (VtyEvent (EvKey KEnter [])) = do
    -- Ignore the keypress if we are already attempting a connection, or
    -- if have already successfully connected but are waiting on the
    -- startup timer.
    case st^.currentState of
        Connecting {} -> return ()
        Idle ->
            case st^.lastAttempt of
                Just (AttemptSucceeded {}) -> return ()
                _ -> do
                    let ci = formState $ st^.loginForm
                    when (populatedConnectionInfo (UsernamePassword ci)) $ do
                        liftIO $ writeBChan (st^.reqChan) $ DoLogin False (UsernamePassword ci)

    continue st
onEvent st e = do
    f' <- handleFormEvent e (st^.loginForm)
    continue $ st & loginForm .~ f'

mkForm :: ConnectionInfoUsernamePassword -> Form ConnectionInfoUsernamePassword e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 18 $ str s <+> fill ' ') <+> w
    in newForm [ label "Server hostname:" @@=
                   editHostname cipHostname Hostname
               , label "Server port:" @@=
                   editShowableField cipPort Port
               , label "Username:" @@=
                   editTextField cipUsername Username (Just 1)
               , label "Password:" @@=
                   editPasswordField cipPassword Password
               ]

editHostname :: (Show n, Ord n) => Lens' s Text -> n -> s -> FormFieldState s e n
editHostname stLens n =
    let ini = id
        val = validHostname
        limit = Just 1
        renderTxt = txt . T.unlines
    in editField stLens n limit ini val renderTxt id

validHostname :: [Text] -> Maybe Text
validHostname ls =
    let s = T.unpack t
        t = T.concat ls
    in if all (flip notElem (":/"::String)) s
       then Just t
       else Nothing

errorAttr :: AttrName
errorAttr = "errorMessage"

colorTheme :: AttrMap
colorTheme = attrMap defAttr
  [ (editAttr, black `on` white)
  , (editFocusedAttr, black `on` yellow)
  , (errorAttr, fg red)
  , (focusedFormInputAttr, black `on` yellow)
  , (invalidFormInputAttr, white `on` red)
  , (clientEmphAttr, fg white `withStyle` bold)
  ]

credsDraw :: State -> [Widget Name]
credsDraw st =
    [ center $ vBox [ if shouldShowForm st then credentialsForm st else emptyWidget
                    , currentStateDisplay st
                    , lastAttemptDisplay (st^.lastAttempt)
                    ]
    ]

-- | Whether the login form should be displayed.
shouldShowForm :: State -> Bool
shouldShowForm st =
    case st^.currentState of
        -- If we're connecting, only show the form if the connection
        -- attempt is not an initial one.
        Connecting initial _ -> not initial

        -- If we're idle, we want to show the form - unless we have
        -- already connected and are waiting for the startup timer to
        -- fire.
        Idle -> case st^.lastAttempt of
            Just (AttemptSucceeded {}) -> st^.timeoutFired
            _ -> True

-- | The "current state" of the login process. Show a connecting status
-- message if a connection is underway, or if one is already established
-- and the startup timer has not fired.
currentStateDisplay :: State -> Widget Name
currentStateDisplay st =
    let msg host = padTop (Pad 1) $ hCenter $
                   txt "Connecting to " <+> withDefAttr clientEmphAttr (txt host) <+> txt "..."
    in case st^.currentState of
          Idle -> case st^.lastAttempt of
              Just (AttemptSucceeded ci _ _ _) -> msg (getHostname ci)
              _ -> emptyWidget
          (Connecting _ host) -> msg host

lastAttemptDisplay :: Maybe LoginAttempt -> Widget Name
lastAttemptDisplay Nothing = emptyWidget
lastAttemptDisplay (Just (AttemptSucceeded {})) = emptyWidget
lastAttemptDisplay (Just (AttemptFailed e)) =
    hCenter $ hLimit uiWidth $
    padTop (Pad 1) $ renderError $ renderText $
    "Error: " <> renderAuthError e

renderAuthError :: AuthenticationException -> Text
renderAuthError (ConnectError _) =
    "Could not connect to server"
renderAuthError (ResolveError _) =
    "Could not resolve server hostname"
renderAuthError (AuthIOError err)
  | Err.isDoesNotExistErrorType (Err.ioeGetErrorType err) =
    "Unable to connect to the network"
  | otherwise = "GetAddrInfo: " <> T.pack (Err.ioeGetErrorString err)
renderAuthError (OtherAuthError e) =
    T.pack $ show e
renderAuthError (LoginError (LoginFailureException msg)) =
    T.pack msg

renderError :: Widget a -> Widget a
renderError = withDefAttr errorAttr

uiWidth :: Int
uiWidth = 60

credentialsForm :: State -> Widget Name
credentialsForm st =
    hCenter $ hLimit uiWidth $ vLimit 15 $
    border $
    vBox [ renderText "Please enter your Mattermost credentials to log in."
         , padTop (Pad 1) $ renderForm (st^.loginForm)
         , hCenter $ renderText "Press Enter to log in or Esc to exit."
         ]
