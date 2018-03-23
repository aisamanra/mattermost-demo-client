{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Types
  ( ConnectionStatus(..)
  , HelpTopic(..)
  , MessageSelectState(..)
  , ProgramOutput(..)
  , MHEvent(..)
  , InternalEvent(..)
  , Name(..)
  , ChannelSelectMatch(..)
  , StartupStateInfo(..)
  , ConnectionInfo(..)
  , ciHostname
  , ciPort
  , ciUsername
  , ciPassword
  , Config(..)
  , HelpScreen(..)
  , PasswordSource(..)
  , MatchType(..)
  , EditMode(..)
  , Mode(..)
  , ChannelSelectPattern(..)
  , PostListContents(..)
  , ChannelSelectMap
  , AuthenticationException(..)
  , BackgroundInfo(..)
  , RequestChan

  , MMNames
  , mkNames
  , refreshChannelZipper
  , getChannelIdsInOrder

  , LinkChoice(LinkChoice)
  , linkUser
  , linkURL
  , linkTime
  , linkName
  , linkFileId

  , ChannelSelectState(..)
  , userMatches
  , channelMatches
  , channelSelectInput
  , selectedMatch
  , emptyChannelSelectState

  , ChatState
  , newState
  , csResources
  , csFocus
  , csCurrentChannel
  , csCurrentChannelId
  , csUrlList
  , csShowMessagePreview
  , csPostMap
  , csRecentChannel
  , csPostListOverlay
  , csUserListOverlay
  , csMyTeam
  , csMessageSelect
  , csJoinChannelList
  , csConnectionStatus
  , csWorkerIsBusy
  , csChannel
  , csChannels
  , csChannelSelectState
  , csEditState
  , csClientConfig
  , timeZone
  , whenMode
  , setMode
  , setMode'
  , appMode

  , ChatEditState
  , emptyEditState
  , cedYankBuffer
  , cedSpellChecker
  , cedMisspellings
  , cedEditMode
  , cedCompleter
  , cedEditor
  , cedMultiline
  , cedInputHistory
  , cedInputHistoryPosition
  , cedLastChannelInput

  , PostListOverlayState
  , postListSelected
  , postListPosts

  , UserSearchScope(..)

  , UserListOverlayState
  , userListSelected
  , userListSearchResults
  , userListSearchInput
  , userListSearchScope
  , userListSearching
  , userListRequestingMore
  , userListHasAllResults
  , userListEnterHandler

  , listFromUserSearchResults

  , ChatResources(ChatResources)
  , crUserPreferences
  , crEventQueue
  , crTheme
  , crSubprocessLog
  , crWebsocketActionChan
  , crRequestQueue
  , crUserStatusLock
  , crUserIdSet
  , crFlaggedPosts
  , crConn
  , crConfiguration
  , getSession
  , getResourceSession

  , UserPreferences(UserPreferences)
  , userPrefShowJoinLeave
  , userPrefFlaggedPostList
  , userPrefGroupChannelPrefs

  , defaultUserPreferences
  , setUserPreferences

  , WebsocketAction(..)

  , Cmd(..)
  , commandName
  , CmdArgs(..)

  , MH
  , runMHEvent
  , mh
  , mhSuspendAndResume
  , mhHandleEventLensed
  , gets

  , requestQuit
  , clientPostToMessage
  , getMessageForPostId
  , getParentMessage
  , resetSpellCheckTimer
  , withChannel
  , withChannelOrDefault
  , userList
  , hasUnread
  , channelNameFromMatch
  , trimAnySigil
  , isMine
  , setUserStatus
  , myUser
  , myUserId
  , myTeamId
  , usernameForUserId
  , userIdForUsername
  , userByDMChannelName
  , userByUsername
  , channelIdByName
  , channelByName
  , userById
  , allUserIds
  , allChannelNames
  , allUsernames
  , sortedUserList
  , removeChannelName
  , addChannelName
  , addNewUser
  , setUserIdSet
  , channelMentionCount
  , useNickname
  , displaynameForUserId
  , raiseInternalEvent

  , userSigil
  , normalChannelSigil

  , HighlightSet(..)
  , UserSet
  , ChannelSet
  , getHighlightSet
  )
where

import           Prelude ()
import           Prelude.Compat

import           Brick (EventM, Next)
import qualified Brick
import           Brick.BChan
import           Brick.AttrMap (AttrMap)
import           Brick.Widgets.Edit (Editor, editor)
import           Brick.Widgets.List (List, list)
import           Control.Applicative ((<|>))
import           Control.Monad (when)
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.MVar (MVar)
import           Control.Exception (SomeException)
import qualified Control.Monad.State as St
import           Control.Monad.State (gets)
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vec
import           Data.HashMap.Strict (HashMap)
import           Data.Time (UTCTime)
import           Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries)
import qualified Data.HashMap.Strict as HM
import           Data.List (sort, partition, sortBy)
import           Data.Maybe
import qualified Data.Set as Set
import           Lens.Micro.Platform ( at, makeLenses, lens, (&), (^.), (%~), (.~), (^?!), (.=)
                                     , (%=), (^?)
                                     , use, _Just, Traversal', preuse, (^..), folded, to, view )
import           Network.Mattermost (ConnectionData)
import           Network.Mattermost.Exceptions
import           Network.Mattermost.Lenses
import           Network.Mattermost.Types
import           Network.Mattermost.Types.Config
import           Network.Mattermost.WebSocket (WebsocketEvent)
import           Network.Connection (HostNotResolved, HostCannotConnect)
import qualified Data.Text as T
import           System.Exit (ExitCode)
import           Text.Aspell (Aspell)

import           Zipper (Zipper, focusL, updateList)

import           InputHistory

import           Types.Channels
import           Types.KeyEvents
import           Types.Posts
import           Types.Messages
import           Types.Users
import           Completion (Completer)

-- * Configuration

-- | A user password is either given to us directly, or a command
-- which we execute to find the password.
data PasswordSource =
    PasswordString T.Text
    | PasswordCommand T.Text
    deriving (Eq, Read, Show)

-- | These are all the values that can be read in our configuration
-- file.
data Config = Config
  { configUser                      :: Maybe T.Text
  , configHost                      :: Maybe T.Text
  , configTeam                      :: Maybe T.Text
  , configPort                      :: Int
  , configPass                      :: Maybe PasswordSource
  , configTimeFormat                :: Maybe T.Text
  , configDateFormat                :: Maybe T.Text
  , configTheme                     :: Maybe T.Text
  , configThemeCustomizationFile    :: Maybe T.Text
  , configSmartBacktick             :: Bool
  , configURLOpenCommand            :: Maybe T.Text
  , configURLOpenCommandInteractive :: Bool
  , configActivityBell              :: Bool
  , configShowBackground            :: BackgroundInfo
  , configShowMessagePreview        :: Bool
  , configEnableAspell              :: Bool
  , configAspellDictionary          :: Maybe T.Text
  , configUnsafeUseHTTP             :: Bool
  , configChannelListWidth          :: Int
  , configShowOlderEdits            :: Bool
  , configShowTypingIndicator       :: Bool
  , configAbsPath                   :: Maybe FilePath
  , configUserKeys                  :: KeyConfig
  , configHyperlinkingMode          :: Bool
  } deriving (Eq, Show)

data BackgroundInfo = Disabled | Active | ActiveCount deriving (Eq, Show)

-- * 'MMNames' structures

-- | The 'MMNames' record is for listing human-readable
--   names and mapping them back to internal IDs.
data MMNames = MMNames
  { _cnChans    :: [T.Text] -- ^ All channel names
  , _cnToChanId :: HashMap T.Text ChannelId
      -- ^ Mapping from channel names to 'ChannelId' values
  , _cnUsers    :: [T.Text] -- ^ All users
  , _cnToUserId :: HashMap T.Text UserId
      -- ^ Mapping from user names to 'UserId' values
  }

mkNames :: User -> HM.HashMap UserId User -> Seq.Seq Channel -> MMNames
mkNames myUser users chans = MMNames
  { _cnChans = sort
               [ preferredChannelName c
               | c <- F.toList chans, channelType c /= Direct ]
  , _cnToChanId = HM.fromList $
                  [ (preferredChannelName c, channelId c) | c <- F.toList chans ] ++
                  [ (userUsername u, c)
                  | u <- HM.elems users
                  , c <- lookupChan (getDMChannelName (getId myUser) (getId u))
                  ]
  , _cnUsers = sort (map userUsername (HM.elems users))
  , _cnToUserId = HM.fromList
                  [ (userUsername u, getId u) | u <- HM.elems users ]
  }
  where lookupChan n = [ c^.channelIdL
                       | c <- F.toList chans, c^.channelNameL == n
                       ]

-- ** 'MMNames' Lenses

makeLenses ''MMNames

-- ** 'MMNames' functions

mkChannelZipperList :: MMNames -> [ChannelId]
mkChannelZipperList chanNames =
  getChannelIdsInOrder chanNames ++
  getDMChannelIdsInOrder chanNames

getChannelIdsInOrder :: MMNames -> [ChannelId]
getChannelIdsInOrder n = [ (n ^. cnToChanId) HM.! i | i <- n ^. cnChans ]

getDMChannelIdsInOrder :: MMNames -> [ChannelId]
getDMChannelIdsInOrder n =
  [ c | i <- n ^. cnUsers
      , c <- maybeToList (HM.lookup i (n ^. cnToChanId))
  ]

-- * Internal Names and References

-- | This 'Name' type is the value used in `brick` to identify the
-- currently focused widget or state.
data Name = ChannelMessages ChannelId
          | MessageInput
          | ChannelList
          | HelpViewport
          | HelpText
          | ScriptHelpText
          | ThemeHelpText
          | KeybindingHelpText
          | ChannelSelectString
          | CompletionAlternatives
          | JoinChannelList
          | UrlList
          | MessagePreviewViewport
          | UserListSearchInput
          | UserListSearchResults
          deriving (Eq, Show, Ord)

-- | The sum type of exceptions we expect to encounter on authentication
-- failure. We encode them explicitly here so that we can print them in
-- a more user-friendly manner than just 'show'.
data AuthenticationException =
    ConnectError HostCannotConnect
    | ResolveError HostNotResolved
    | AuthIOError IOError
    | LoginError LoginFailureException
    | OtherAuthError SomeException
    deriving (Show)

-- | Our 'ConnectionInfo' contains exactly as much information as is
-- necessary to start a connection with a Mattermost server
data ConnectionInfo =
    ConnectionInfo { _ciHostname :: T.Text
                   , _ciPort     :: Int
                   , _ciUsername :: T.Text
                   , _ciPassword :: T.Text
                   }

makeLenses ''ConnectionInfo

-- | We want to continue referring to posts by their IDs, but we don't want to
-- have to synthesize new valid IDs for messages from the client
-- itself (like error messages or informative client responses). To
-- that end, a PostRef can be either a PostId or a newly-generated
-- client ID
data PostRef
  = MMId PostId
  | CLId Int
    deriving (Eq, Show)

-- | For representing links to things in the 'open links' view
data LinkChoice = LinkChoice
  { _linkTime   :: ServerTime
  , _linkUser   :: UserRef
  , _linkName   :: T.Text
  , _linkURL    :: T.Text
  , _linkFileId :: Maybe FileId
  } deriving (Eq, Show)

makeLenses ''LinkChoice

-- Sigils
normalChannelSigil :: T.Text
normalChannelSigil = "~"

userSigil :: T.Text
userSigil = "@"

-- ** Channel-matching types

data ChannelSelectMatch =
    ChannelSelectMatch { nameBefore     :: T.Text
                       , nameMatched    :: T.Text
                       , nameAfter      :: T.Text
                       }
                       deriving (Eq, Show)

channelNameFromMatch :: ChannelSelectMatch -> T.Text
channelNameFromMatch (ChannelSelectMatch b m a) = b <> m <> a

data ChannelSelectPattern = CSP MatchType T.Text
                          deriving (Eq, Show)

data MatchType = Prefix | Suffix | Infix | Equal deriving (Eq, Show)


-- * Application State Values

data ProgramOutput =
    ProgramOutput { program :: FilePath
                  , programArgs :: [String]
                  , programStdout :: String
                  , programStdoutExpected :: Bool
                  , programStderr :: String
                  , programExitCode :: ExitCode
                  }

data UserPreferences = UserPreferences
  { _userPrefShowJoinLeave     :: Bool
  , _userPrefFlaggedPostList   :: Seq.Seq FlaggedPost
  , _userPrefGroupChannelPrefs :: HM.HashMap ChannelId Bool
  }

defaultUserPreferences :: UserPreferences
defaultUserPreferences = UserPreferences
  { _userPrefShowJoinLeave     = True
  , _userPrefFlaggedPostList   = mempty
  , _userPrefGroupChannelPrefs = mempty
  }

setUserPreferences :: Seq.Seq Preference -> UserPreferences -> UserPreferences
setUserPreferences = flip (F.foldr go)
  where go p u
          | Just fp <- preferenceToFlaggedPost p =
            u { _userPrefFlaggedPostList =
                _userPrefFlaggedPostList u Seq.|> fp
              }
          | Just gp <- preferenceToGroupChannelPreference p =
            u { _userPrefGroupChannelPrefs =
                HM.insert
                  (groupChannelId gp)
                  (groupChannelShow gp)
                  (_userPrefGroupChannelPrefs u)
              }
          | preferenceName p == PreferenceName "join_leave" =
            u { _userPrefShowJoinLeave =
                preferenceValue p /= PreferenceValue "false" }
          | otherwise = u

-- | 'ChatResources' represents configuration and
-- connection-related information, as opposed to
-- current model or view information. Information
-- that goes in the 'ChatResources' value should be
-- limited to information that we read or set up
-- prior to setting up the bulk of the application state.
data ChatResources = ChatResources
  { _crSession             :: Session
  , _crConn                :: ConnectionData
  , _crRequestQueue        :: RequestChan
  , _crEventQueue          :: BChan MHEvent
  , _crSubprocessLog       :: STM.TChan ProgramOutput
  , _crWebsocketActionChan :: STM.TChan WebsocketAction
  , _crTheme               :: AttrMap
  , _crUserStatusLock      :: MVar ()
  , _crUserIdSet           :: STM.TVar (Seq.Seq UserId)
  , _crConfiguration       :: Config
  , _crFlaggedPosts        :: Set.Set PostId
  , _crUserPreferences     :: UserPreferences
  }


-- | The 'ChatEditState' value contains the editor widget itself
--   as well as history and metadata we need for editing-related
--   operations.
data ChatEditState = ChatEditState
  { _cedEditor               :: Editor T.Text Name
  , _cedEditMode             :: EditMode
  , _cedMultiline            :: Bool
  , _cedInputHistory         :: InputHistory
  , _cedInputHistoryPosition :: HM.HashMap ChannelId (Maybe Int)
  , _cedLastChannelInput     :: HM.HashMap ChannelId (T.Text, EditMode)
  , _cedCompleter            :: Maybe Completer
  , _cedYankBuffer           :: T.Text
  , _cedSpellChecker         :: Maybe (Aspell, IO ())
  , _cedMisspellings         :: Set.Set T.Text
  }

data EditMode =
    NewPost
    | Editing Post
    | Replying Message Post
      deriving (Show)

-- | We can initialize a new 'ChatEditState' value with just an
--   edit history, which we save locally.
emptyEditState :: InputHistory -> Maybe (Aspell, IO ()) -> ChatEditState
emptyEditState hist sp = ChatEditState
  { _cedEditor               = editor MessageInput Nothing ""
  , _cedMultiline            = False
  , _cedInputHistory         = hist
  , _cedInputHistoryPosition = mempty
  , _cedLastChannelInput     = mempty
  , _cedCompleter            = Nothing
  , _cedEditMode             = NewPost
  , _cedYankBuffer           = ""
  , _cedSpellChecker         = sp
  , _cedMisspellings         = mempty
  }

-- | A 'RequestChan' is a queue of operations we have to perform
--   in the background to avoid blocking on the main loop
type RequestChan = STM.TChan (IO (MH ()))

-- | The 'HelpScreen' type represents the set of possible 'Help'
--   dialogues we have to choose from.
data HelpScreen
  = MainHelp
  | ScriptHelp
  | ThemeHelp
  | KeybindingHelp
    deriving (Eq)

-- |  Help topics
data HelpTopic =
    HelpTopic { helpTopicName         :: T.Text
              , helpTopicDescription  :: T.Text
              , helpTopicScreen       :: HelpScreen
              , helpTopicViewportName :: Name
              }
              deriving (Eq)

-- | Mode type for the current contents of the post list overlay
data PostListContents
  = PostListFlagged
  | PostListSearch T.Text Bool -- for the query and search status
  --   | PostListPinned ChannelId
  deriving (Eq)

-- | The 'Mode' represents the current dominant UI activity
data Mode =
    Main
    | ShowHelp HelpTopic
    | ChannelSelect
    | UrlSelect
    | LeaveChannelConfirm
    | DeleteChannelConfirm
    | JoinChannel
    | ChannelScroll
    | MessageSelect
    | MessageSelectDeleteConfirm
    | PostListOverlay PostListContents
    | UserListOverlay
    deriving (Eq)

-- | We're either connected or we're not.
data ConnectionStatus = Connected | Disconnected

-- | This is the giant bundle of fields that represents the current
--  state of our application at any given time. Some of this should
--  be broken out further, but hasn't yet been.
data ChatState = ChatState
  { _csResources                   :: ChatResources
  , _csFocus                       :: Zipper ChannelId
  , _csNames                       :: MMNames
  , _csMe                          :: User
  , _csMyTeam                      :: Team
  , _csChannels                    :: ClientChannels
  , _csPostMap                     :: HashMap PostId Message
  , _csUsers                       :: Users
  , _timeZone                      :: TimeZoneSeries
  , _csEditState                   :: ChatEditState
  , _csMode                        :: Mode
  , _csShowMessagePreview          :: Bool
  , _csChannelSelectState          :: ChannelSelectState
  , _csRecentChannel               :: Maybe ChannelId
  , _csUrlList                     :: List Name LinkChoice
  , _csConnectionStatus            :: ConnectionStatus
  , _csWorkerIsBusy                :: Maybe (Maybe Int)
  , _csJoinChannelList             :: Maybe (List Name Channel)
  , _csMessageSelect               :: MessageSelectState
  , _csPostListOverlay             :: PostListOverlayState
  , _csUserListOverlay             :: UserListOverlayState
  , _csClientConfig                :: Maybe ClientConfig
  }

data StartupStateInfo =
    StartupStateInfo { startupStateResources      :: ChatResources
                     , startupStateChannelZipper  :: Zipper ChannelId
                     , startupStateConnectedUser  :: User
                     , startupStateTeam           :: Team
                     , startupStateTimeZone       :: TimeZoneSeries
                     , startupStateInitialHistory :: InputHistory
                     , startupStateSpellChecker   :: Maybe (Aspell, IO ())
                     , startupStateNames          :: MMNames
                     }

newState :: StartupStateInfo -> ChatState
newState (StartupStateInfo rs i u m tz hist sp ns) = ChatState
  { _csResources                   = rs
  , _csFocus                       = i
  , _csMe                          = u
  , _csMyTeam                      = m
  , _csNames                       = ns
  , _csChannels                    = noChannels
  , _csPostMap                     = HM.empty
  , _csUsers                       = noUsers
  , _timeZone                      = tz
  , _csEditState                   = emptyEditState hist sp
  , _csMode                        = Main
  , _csShowMessagePreview          = configShowMessagePreview $ _crConfiguration rs
  , _csChannelSelectState          = emptyChannelSelectState
  , _csRecentChannel               = Nothing
  , _csUrlList                     = list UrlList mempty 2
  , _csConnectionStatus            = Connected
  , _csWorkerIsBusy                = Nothing
  , _csJoinChannelList             = Nothing
  , _csMessageSelect               = MessageSelectState Nothing
  , _csPostListOverlay             = PostListOverlayState mempty Nothing
  , _csUserListOverlay             = nullUserListOverlayState
  , _csClientConfig                = Nothing
  }

nullUserListOverlayState :: UserListOverlayState
nullUserListOverlayState =
    UserListOverlayState { _userListSearchResults = listFromUserSearchResults mempty
                         , _userListSelected    = Nothing
                         , _userListSearchInput = editor UserListSearchInput (Just 1) ""
                         , _userListSearchScope = AllUsers
                         , _userListSearching = False
                         , _userListRequestingMore = False
                         , _userListHasAllResults = False
                         , _userListEnterHandler = const $ return False
                         }

listFromUserSearchResults :: Vec.Vector UserInfo -> List Name UserInfo
listFromUserSearchResults rs =
    -- NB: The item height here needs to actually match the UI drawing
    -- in Draw.UserListOverlay.
    list UserListSearchResults rs 1

type ChannelSelectMap = HM.HashMap T.Text ChannelSelectMatch

data ChannelSelectState =
    ChannelSelectState { _channelSelectInput :: T.Text
                       , _channelMatches     :: ChannelSelectMap
                       , _userMatches        :: ChannelSelectMap
                       , _selectedMatch      :: T.Text
                       }

emptyChannelSelectState :: ChannelSelectState
emptyChannelSelectState =
    ChannelSelectState { _channelSelectInput = ""
                       , _channelMatches     = mempty
                       , _userMatches        = mempty
                       , _selectedMatch      = ""
                       }

data MessageSelectState =
    MessageSelectState { selectMessagePostId :: Maybe PostId }

data PostListOverlayState = PostListOverlayState
  { _postListPosts    :: Messages
  , _postListSelected :: Maybe PostId
  }

data UserListOverlayState = UserListOverlayState
  { _userListSearchResults :: List Name UserInfo
  , _userListSelected :: Maybe PostId
  , _userListSearchInput :: Editor T.Text Name
  , _userListSearchScope :: UserSearchScope
  , _userListSearching :: Bool
  , _userListRequestingMore :: Bool
  , _userListHasAllResults :: Bool
  , _userListEnterHandler :: UserInfo -> MH Bool
  }

data UserSearchScope =
    ChannelMembers ChannelId
    | ChannelNonMembers ChannelId
    | AllUsers

-- | Actions that can be sent on the websocket to the server.
data WebsocketAction =
    UserTyping UTCTime ChannelId (Maybe PostId) -- ^ user typing in the input box
  -- | GetStatuses
  -- | GetStatusesByIds [UserId]
  deriving (Read, Show, Eq, Ord)

-- * MH Monad

-- | A value of type 'MH' @a@ represents a computation that can
-- manipulate the application state and also request that the
-- application quit
newtype MH a =
  MH { fromMH :: St.StateT (ChatState, ChatState -> EventM Name (Next ChatState))
                           (EventM Name) a }

-- | Run an 'MM' computation, choosing whether to continue or halt
--   based on the resulting
runMHEvent :: ChatState -> MH () -> EventM Name (Next ChatState)
runMHEvent st (MH mote) = do
  ((), (st', rs)) <- St.runStateT mote (st, Brick.continue)
  rs st'

-- | lift a computation in 'EventM' into 'MH'
mh :: EventM Name a -> MH a
mh = MH . St.lift

mhHandleEventLensed :: Lens' ChatState b -> (e -> b -> EventM Name b) -> e -> MH ()
mhHandleEventLensed ln f event = MH $ do
  (st, b) <- St.get
  n <- St.lift $ f event (st ^. ln)
  St.put (st & ln .~ n , b)

mhSuspendAndResume :: (ChatState -> IO ChatState) -> MH ()
mhSuspendAndResume mote = MH $ do
  (st, _) <- St.get
  St.put (st, \ _ -> Brick.suspendAndResume (mote st))

-- | This will request that after this computation finishes the
-- application should exit
requestQuit :: MH ()
requestQuit = MH $ do
  (st, _) <- St.get
  St.put (st, Brick.halt)

instance Functor MH where
  fmap f (MH x) = MH (fmap f x)

instance Applicative MH where
  pure x = MH (pure x)
  MH f <*> MH x = MH (f <*> x)

instance Monad MH where
  return x = MH (return x)
  MH x >>= f = MH (x >>= \ x' -> fromMH (f x'))

-- We want to pretend that the state is only the ChatState, rather
-- than the ChatState and the Brick continuation
instance St.MonadState ChatState MH where
  get = fst `fmap` MH St.get
  put st = MH $ do
    (_, c) <- St.get
    St.put (st, c)

instance St.MonadIO MH where
  liftIO = MH . St.liftIO

-- | This represents events that we handle in the main application loop.
data MHEvent
    = WSEvent WebsocketEvent
    -- ^ For events that arise from the websocket
    | RespEvent (MH ())
    -- ^ For the result values of async IO operations
    | AsyncErrEvent SomeException
    -- ^ For errors that arise in the course of async IO operations
    | RefreshWebsocketEvent
    -- ^ Tell our main loop to refresh the websocket connection
    | WebsocketParseError String
    -- ^ We failed to parse an incoming websocket event
    | WebsocketDisconnect
    -- ^ The websocket connection went down.
    | WebsocketConnect
    -- ^ The websocket connection came up.
    | BGIdle
    -- ^ background worker is idle
    | BGBusy (Maybe Int)
    -- ^ background worker is busy (with n requests)
    | IEvent InternalEvent
    -- ^ MH-internal events

data InternalEvent
    = DisplayError T.Text
      -- ^ Display a generic error message to the user
    deriving (Eq, Show)

-- ** Application State Lenses

makeLenses ''ChatResources
makeLenses ''ChatState
makeLenses ''ChatEditState
makeLenses ''PostListOverlayState
makeLenses ''UserListOverlayState
makeLenses ''ChannelSelectState
makeLenses ''UserPreferences

getSession :: MH Session
getSession = use (csResources.crSession)

getResourceSession :: ChatResources -> Session
getResourceSession = _crSession

whenMode :: Mode -> MH () -> MH ()
whenMode m act = do
    curMode <- use csMode
    when (curMode == m) act

setMode :: Mode -> MH ()
setMode = (csMode .=)

setMode' :: Mode -> ChatState -> ChatState
setMode' m st = st & csMode .~ m

appMode :: ChatState -> Mode
appMode = _csMode

resetSpellCheckTimer :: ChatEditState -> IO ()
resetSpellCheckTimer s =
    case s^.cedSpellChecker of
        Nothing -> return ()
        Just (_, reset) -> reset

-- ** Utility Lenses
csCurrentChannelId :: Lens' ChatState ChannelId
csCurrentChannelId = csFocus.focusL

csCurrentChannel :: Lens' ChatState ClientChannel
csCurrentChannel =
  lens (\ st -> findChannelById (st^.csCurrentChannelId) (st^.csChannels) ^?! _Just)
       (\ st n -> st & csChannels %~ addChannel (st^.csCurrentChannelId) n)

csChannel :: ChannelId -> Traversal' ChatState ClientChannel
csChannel cId =
  csChannels . channelByIdL cId

withChannel :: ChannelId -> (ClientChannel -> MH ()) -> MH ()
withChannel cId = withChannelOrDefault cId ()

withChannelOrDefault :: ChannelId -> a -> (ClientChannel -> MH a) -> MH a
withChannelOrDefault cId deflt mote = do
  chan <- preuse (csChannel(cId))
  case chan of
    Nothing -> return deflt
    Just c  -> mote c

-- ** 'ChatState' Helper Functions

raiseInternalEvent :: InternalEvent -> MH ()
raiseInternalEvent ev = do
  queue <- use (csResources.crEventQueue)
  St.liftIO $ writeBChan queue (IEvent ev)

isMine :: ChatState -> Message -> Bool
isMine st msg = (UserI $ myUserId st) == msg^.mUser

getMessageForPostId :: ChatState -> PostId -> Maybe Message
getMessageForPostId st pId = st^.csPostMap.at(pId)

getParentMessage :: ChatState -> Message -> Maybe Message
getParentMessage st msg
  | InReplyTo pId <- msg^.mInReplyToMsg
    = st^.csPostMap.at(pId)
  | otherwise = Nothing

setUserStatus :: UserId -> T.Text -> MH ()
setUserStatus uId t = csUsers %= modifyUserById uId (uiStatus .~ statusFromText t)

nicknameForUserId :: UserId -> ChatState -> Maybe T.Text
nicknameForUserId uId st = _uiNickName =<< findUserById uId (st^.csUsers)

usernameForUserId :: UserId -> ChatState -> Maybe T.Text
usernameForUserId uId st = _uiName <$> findUserById uId (st^.csUsers)

displaynameForUserId :: UserId -> ChatState -> Maybe T.Text
displaynameForUserId uId st
    | useNickname st =
        nicknameForUserId uId st <|> usernameForUserId uId st
    | otherwise =
        usernameForUserId uId st

userIdForUsername :: T.Text -> ChatState -> Maybe UserId
userIdForUsername name st = st^.csNames.cnToUserId.at name

channelIdByName :: T.Text -> ChatState -> Maybe ChannelId
channelIdByName name st =
    let userInfos = st^.csUsers.to allUsers
        uName = if useNickname st
                then
                    maybe name (view uiName)
                              $ findUserByNickname userInfos name
                else name
        nameToChanId = st^.csNames.cnToChanId
    in HM.lookup (trimAnySigil uName) nameToChanId

useNickname :: ChatState -> Bool
useNickname st = case st^?csClientConfig._Just.to clientConfigTeammateNameDisplay of
                      Just "nickname_full_name" ->
                          True
                      _ ->
                          False

channelByName :: T.Text -> ChatState -> Maybe ClientChannel
channelByName n st = do
    let userInfos = st^.csUsers.to allUsers
        uName = if useNickname st
              then
                  maybe n (view uiName)
                            $ findUserByNickname userInfos n
              else n
    cId <- channelIdByName uName st
    findChannelById cId (st^.csChannels)

trimAnySigil :: T.Text -> T.Text
trimAnySigil n
    | normalChannelSigil `T.isPrefixOf` n = T.tail n
    | userSigil `T.isPrefixOf` n          = T.tail n
    | otherwise                           = n

addNewUser :: UserInfo -> MH ()
addNewUser u = do
    csUsers %= addUser u

    let uname = u^.uiName
        uid = u^.uiId
    csNames.cnUsers %= (sort . (uname:))
    csNames.cnToUserId.at uname .= Just uid

    userSet <- use (csResources.crUserIdSet)
    St.liftIO $ STM.atomically $ STM.modifyTVar userSet $ (uid Seq.<|)

setUserIdSet :: Seq.Seq UserId -> MH ()
setUserIdSet ids = do
    userSet <- use (csResources.crUserIdSet)
    St.liftIO $ STM.atomically $ STM.writeTVar userSet ids

addChannelName :: Type -> ChannelId -> T.Text -> MH ()
addChannelName chType cid name = do
    csNames.cnToChanId.at(name) .= Just cid

    -- For direct channels the username is already in the user list so
    -- do nothing
    existingNames <- gets allChannelNames
    when (chType /= Direct && (not $ name `elem` existingNames)) $
        csNames.cnChans %= (sort . (name:))

channelMentionCount :: ChannelId -> ChatState -> Int
channelMentionCount cId st =
    maybe 0 id (st^?csChannel(cId).ccInfo.cdMentionCount)

allChannelNames :: ChatState -> [T.Text]
allChannelNames st = st^.csNames.cnChans

allUsernames :: ChatState -> [T.Text]
allUsernames st = st^.csNames.cnChans

removeChannelName :: T.Text -> MH ()
removeChannelName name = do
    -- Flush cnToChanId
    csNames.cnToChanId.at name .= Nothing
    -- Flush cnChans
    csNames.cnChans %= filter (/= name)

-- Rebuild the channel zipper contents from the current names collection.
refreshChannelZipper :: MH ()
refreshChannelZipper = do
    -- We should figure out how to do this better: this adds it to the
    -- channel zipper in such a way that we don't ever change our focus
    -- to something else, which is kind of silly
    names <- use csNames
    let newZip = updateList (mkChannelZipperList names)
    csFocus %= newZip

clientPostToMessage :: ClientPost -> Message
clientPostToMessage cp = Message
  { _mText          = cp^.cpText
  , _mUser          = case cp^.cpUserOverride of
                        Just n | cp^.cpType == NormalPost -> UserOverride (n <> "[BOT]")
                        _ -> maybe NoUser UserI $ cp^.cpUser
  , _mDate          = cp^.cpDate
  , _mType          = CP $ cp^.cpType
  , _mPending       = cp^.cpPending
  , _mDeleted       = cp^.cpDeleted
  , _mAttachments   = cp^.cpAttachments
  , _mInReplyToMsg  =
    case cp^.cpInReplyToPost of
      Nothing  -> NotAReply
      Just pId -> InReplyTo pId
  , _mPostId        = Just $ cp^.cpPostId
  , _mReactions     = cp^.cpReactions
  , _mOriginalPost  = Just $ cp^.cpOriginalPost
  , _mFlagged       = False
  , _mChannelId     = Just $ cp^.cpChannelId
  }

-- * Slash Commands

-- | The 'CmdArgs' type represents the arguments to a slash-command;
--   the type parameter represents the argument structure.
data CmdArgs :: * -> * where
  NoArg    :: CmdArgs ()
  LineArg  :: T.Text -> CmdArgs T.Text
  TokenArg :: T.Text -> CmdArgs rest -> CmdArgs (T.Text, rest)

-- | A 'CmdExec' value represents the implementation of a command
--   when provided with its arguments
type CmdExec a = a -> MH ()

-- | A 'Cmd' packages up a 'CmdArgs' specifier and the 'CmdExec'
--   implementation with a name and a description.
data Cmd = forall a. Cmd
  { cmdName    :: T.Text
  , cmdDescr   :: T.Text
  , cmdArgSpec :: CmdArgs a
  , cmdAction  :: CmdExec a
  }

-- | Helper function to extract the name out of a 'Cmd' value
commandName :: Cmd -> T.Text
commandName (Cmd name _ _ _ ) = name

-- *  Channel Updates and Notifications

hasUnread :: ChatState -> ChannelId -> Bool
hasUnread st cId = maybe False id $ do
  chan <- findChannelById cId (st^.csChannels)
  lastViewTime <- chan^.ccInfo.cdViewed
  return $ (chan^.ccInfo.cdUpdated > lastViewTime) ||
           (isJust $ chan^.ccInfo.cdEditedMessageThreshold)

userList :: ChatState -> [UserInfo]
userList st = filter showUser $ allUsers (st^.csUsers)
  where showUser u = not (isSelf u) && (u^.uiInTeam)
        isSelf u = (myUserId st) == (u^.uiId)

allUserIds :: ChatState -> [UserId]
allUserIds st = getAllUserIds $ st^.csUsers

userById :: UserId -> ChatState -> Maybe UserInfo
userById uId st = findUserById uId (st^.csUsers)

myUserId :: ChatState -> UserId
myUserId st = myUser st ^. userIdL

myTeamId :: ChatState -> TeamId
myTeamId st = st ^. csMyTeam . teamIdL

myUser :: ChatState -> User
myUser st = st^.csMe

userByDMChannelName :: T.Text
                    -- ^ the dm channel name
                    -> UserId
                    -- ^ me
                    -> ChatState
                    -> Maybe UserInfo
                    -- ^ you
userByDMChannelName name self st =
    findUserByDMChannelName (st^.csUsers) name self

userByUsername :: T.Text -> ChatState -> Maybe UserInfo
userByUsername name st = do
    uId <- userIdForUsername name st
    userById uId st

sortedUserList :: ChatState -> [UserInfo]
sortedUserList st = sortBy cmp yes <> sortBy cmp no
  where
      cmp = compareUserInfo uiName
      dmHasUnread u =
          case st^.csNames.cnToChanId.at(u^.uiName) of
            Nothing  -> False
            Just cId
              | (st^.csCurrentChannelId) == cId -> False
              | otherwise -> hasUnread st cId
      (yes, no) = partition dmHasUnread (filter (not . _uiDeleted) $ userList st)

compareUserInfo :: (Ord a) => Lens' UserInfo a -> UserInfo -> UserInfo -> Ordering
compareUserInfo field u1 u2
    | u1^.uiStatus == Offline && u2^.uiStatus /= Offline =
      GT
    | u1^.uiStatus /= Offline && u2^.uiStatus == Offline =
      LT
    | otherwise =
      (u1^.field) `compare` (u2^.field)

-- * HighlightSet

type UserSet = Set.Set T.Text
type ChannelSet = Set.Set T.Text

data HighlightSet = HighlightSet
  { hUserSet    :: Set.Set T.Text
  , hChannelSet :: Set.Set T.Text
  }

getHighlightSet :: ChatState -> HighlightSet
getHighlightSet st = HighlightSet
  { hUserSet = Set.fromList (st^..csUsers.to allUsers.folded.uiName)
  , hChannelSet = Set.fromList (st^..csChannels.folded.ccInfo.cdName)
  }
