module Types.KeyEvents
  (
  -- * Types
    KeyEvent(..)
  , KeyConfig
  , Binding(..)
  , BindingState(..)

  -- * Data
  , allEvents

  -- * Parsing and pretty-printing
  , parseBinding
  , parseBindingList
  , ppBinding
  , nonCharKeys
  , eventToBinding

  -- * Key event name resolution
  , keyEventFromName
  , keyEventName
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as Vty


-- | This enum represents all the possible key events a user might
--   want to use.
data KeyEvent
  = VtyRefreshEvent
  | ShowHelpEvent
  | EnterSelectModeEvent
  | ReplyRecentEvent
  | ToggleMessagePreviewEvent
  | InvokeEditorEvent
  | EnterFastSelectModeEvent
  | QuitEvent
  | NextChannelEvent
  | PrevChannelEvent
  | NextUnreadChannelEvent
  | NextUnreadUserOrChannelEvent
  | LastChannelEvent
  | EnterOpenURLModeEvent
  | ClearUnreadEvent
  | ToggleMultiLineEvent
  | EnterFlaggedPostsEvent
  | ToggleChannelListVisibleEvent
  | ShowAttachmentListEvent

  | SelectNextTabEvent
  | SelectPreviousTabEvent

  -- generic cancel
  | CancelEvent

  -- channel-scroll-specific
  | LoadMoreEvent
  | OpenMessageURLEvent

  -- scrolling events---maybe rebindable?
  | ScrollUpEvent
  | ScrollDownEvent
  | ScrollLeftEvent
  | ScrollRightEvent
  | PageUpEvent
  | PageDownEvent
  | ScrollTopEvent
  | ScrollBottomEvent

  -- select events---not the same as scrolling sometimes!
  | SelectUpEvent
  | SelectDownEvent

  -- search select events---these need to not be valid editor inputs
  -- (such as 'j' and 'k')
  | SearchSelectUpEvent
  | SearchSelectDownEvent

  -- E.g. Pressing enter on an item in a list to do something with it
  | ActivateListItemEvent

  | ViewMessageEvent
  | FillGapEvent
  | FlagMessageEvent
  | PinMessageEvent
  | YankMessageEvent
  | YankWholeMessageEvent
  | DeleteMessageEvent
  | EditMessageEvent
  | ReplyMessageEvent
  | ReactToMessageEvent

  -- Attachments
  | AttachmentListAddEvent
  | AttachmentListDeleteEvent
  | AttachmentOpenEvent
    deriving (Eq, Show, Ord, Enum)

allEvents :: [KeyEvent]
allEvents =
  [ QuitEvent
  , VtyRefreshEvent
  , ClearUnreadEvent

  , ToggleMessagePreviewEvent
  , InvokeEditorEvent
  , ToggleMultiLineEvent
  , CancelEvent
  , ReplyRecentEvent
  , SelectNextTabEvent
  , SelectPreviousTabEvent

  , EnterFastSelectModeEvent
  , NextChannelEvent
  , PrevChannelEvent
  , NextUnreadChannelEvent
  , NextUnreadUserOrChannelEvent
  , LastChannelEvent

  , ShowAttachmentListEvent

  , EnterFlaggedPostsEvent
  , ToggleChannelListVisibleEvent
  , ShowHelpEvent
  , EnterSelectModeEvent
  , EnterOpenURLModeEvent

  , LoadMoreEvent
  , OpenMessageURLEvent

  , ScrollUpEvent
  , ScrollDownEvent
  , ScrollLeftEvent
  , ScrollRightEvent
  , PageUpEvent
  , PageDownEvent
  , ScrollTopEvent
  , ScrollBottomEvent

  , SelectUpEvent
  , SelectDownEvent

  , ActivateListItemEvent

  , SearchSelectUpEvent
  , SearchSelectDownEvent

  , FlagMessageEvent
  , PinMessageEvent
  , ViewMessageEvent
  , FillGapEvent
  , YankMessageEvent
  , YankWholeMessageEvent
  , DeleteMessageEvent
  , EditMessageEvent
  , ReplyMessageEvent
  , ReactToMessageEvent
  , AttachmentListAddEvent
  , AttachmentListDeleteEvent
  , AttachmentOpenEvent
  ]

eventToBinding :: Vty.Event -> Binding
eventToBinding (Vty.EvKey k mods) = Binding mods k
eventToBinding k = error $ "BUG: invalid keybinding " <> show k

data Binding = Binding
  { kbMods :: [Vty.Modifier]
  , kbKey  :: Vty.Key
  } deriving (Eq, Show, Ord)

data BindingState =
    BindingList [Binding]
    | Unbound
    deriving (Show, Eq, Ord)

type KeyConfig = M.Map KeyEvent BindingState

parseBinding :: Text -> Either String Binding
parseBinding kb = go (T.splitOn "-" $ T.toLower kb) []
  where go [k] mods = do
          key <- pKey k
          return Binding { kbMods = mods, kbKey = key }
        go (k:ks) mods = do
          m <- case k of
            "s"       -> return Vty.MShift
            "shift"   -> return Vty.MShift
            "m"       -> return Vty.MMeta
            "meta"    -> return Vty.MMeta
            "a"       -> return Vty.MAlt
            "alt"     -> return Vty.MAlt
            "c"       -> return Vty.MCtrl
            "ctrl"    -> return Vty.MCtrl
            "control" -> return Vty.MCtrl
            _         -> Left ("Unknown modifier prefix: " ++ show k)
          go ks (m:mods)
        go [] _ = Left "Empty keybinding not allowed"
        pKey "esc"       = return Vty.KEsc
        pKey "backspace" = return Vty.KBS
        pKey "enter"     = return Vty.KEnter
        pKey "left"      = return Vty.KLeft
        pKey "right"     = return Vty.KRight
        pKey "up"        = return Vty.KUp
        pKey "down"      = return Vty.KDown
        pKey "upleft"    = return Vty.KUpLeft
        pKey "upright"   = return Vty.KUpRight
        pKey "downleft"  = return Vty.KDownLeft
        pKey "downright" = return Vty.KDownRight
        pKey "center"    = return Vty.KCenter
        pKey "backtab"   = return Vty.KBackTab
        pKey "printscreen" = return Vty.KPrtScr
        pKey "pause"     = return Vty.KPause
        pKey "insert"    = return Vty.KIns
        pKey "home"      = return Vty.KHome
        pKey "pgup"      = return Vty.KPageUp
        pKey "del"       = return Vty.KDel
        pKey "end"       = return Vty.KEnd
        pKey "pgdown"    = return Vty.KPageDown
        pKey "begin"     = return Vty.KBegin
        pKey "menu"      = return Vty.KMenu
        pKey "space"     = return (Vty.KChar ' ')
        pKey "tab"       = return (Vty.KChar '\t')
        pKey t
          | Just (c, "") <- T.uncons t =
              return (Vty.KChar c)
          | Just n <- T.stripPrefix "f" t =
              case readMaybe (T.unpack n) of
                  Nothing -> Left ("Unknown keybinding: " ++ show t)
                  Just i -> return (Vty.KFun i)
          | otherwise = Left ("Unknown keybinding: " ++ show t)

ppBinding :: Binding -> Text
ppBinding (Binding mods k) =
    T.intercalate "-" $ (ppMod <$> mods) <> [ppKey k]

ppKey :: Vty.Key -> Text
ppKey (Vty.KChar c)   = ppChar c
ppKey (Vty.KFun n)    = "F" <> (T.pack $ show n)
ppKey Vty.KBackTab    = "BackTab"
ppKey Vty.KEsc        = "Esc"
ppKey Vty.KBS         = "Backspace"
ppKey Vty.KEnter      = "Enter"
ppKey Vty.KUp         = "Up"
ppKey Vty.KDown       = "Down"
ppKey Vty.KLeft       = "Left"
ppKey Vty.KRight      = "Right"
ppKey Vty.KHome       = "Home"
ppKey Vty.KEnd        = "End"
ppKey Vty.KPageUp     = "PgUp"
ppKey Vty.KPageDown   = "PgDown"
ppKey Vty.KDel        = "Del"
ppKey Vty.KUpLeft     = "UpLeft"
ppKey Vty.KUpRight    = "UpRight"
ppKey Vty.KDownLeft   = "DownLeft"
ppKey Vty.KDownRight  = "DownRight"
ppKey Vty.KCenter     = "Center"
ppKey Vty.KPrtScr     = "PrintScreen"
ppKey Vty.KPause      = "Pause"
ppKey Vty.KIns        = "Insert"
ppKey Vty.KBegin      = "Begin"
ppKey Vty.KMenu       = "Menu"

nonCharKeys :: [Text]
nonCharKeys = map ppKey
  [ Vty.KBackTab, Vty.KEsc, Vty.KBS, Vty.KEnter, Vty.KUp, Vty.KDown
  , Vty.KLeft, Vty.KRight, Vty.KHome, Vty.KEnd, Vty.KPageDown
  , Vty.KPageUp, Vty.KDel, Vty.KUpLeft, Vty.KUpRight, Vty.KDownLeft
  , Vty.KDownRight, Vty.KCenter, Vty.KPrtScr, Vty.KPause, Vty.KIns
  , Vty.KBegin, Vty.KMenu
  ]

ppChar :: Char -> Text
ppChar '\t' = "Tab"
ppChar ' '  = "Space"
ppChar c    = T.singleton c

ppMod :: Vty.Modifier -> Text
ppMod Vty.MMeta  = "M"
ppMod Vty.MAlt   = "A"
ppMod Vty.MCtrl  = "C"
ppMod Vty.MShift = "S"

parseBindingList :: Text -> Either String BindingState
parseBindingList t =
    if T.toLower t == "unbound"
    then return Unbound
    else BindingList <$> mapM (parseBinding . T.strip) (T.splitOn "," t)

keyEventFromName :: Text -> Either String KeyEvent
keyEventFromName t =
    let mapping = M.fromList [ (keyEventName e, e) | e <- allEvents ]
    in case M.lookup t mapping of
        Just e -> return e
        Nothing -> Left ("Unknown event: " ++ show t)

keyEventName :: KeyEvent -> Text
keyEventName ev = case ev of
  QuitEvent                 -> "quit"
  VtyRefreshEvent           -> "vty-refresh"
  ClearUnreadEvent          -> "clear-unread"
  CancelEvent               -> "cancel"

  ToggleMessagePreviewEvent -> "toggle-message-preview"
  InvokeEditorEvent         -> "invoke-editor"
  ToggleMultiLineEvent      -> "toggle-multiline"
  ReplyRecentEvent          -> "reply-recent"

  EnterFastSelectModeEvent  -> "enter-fast-select"
  NextChannelEvent          -> "focus-next-channel"
  PrevChannelEvent          -> "focus-prev-channel"
  NextUnreadChannelEvent    -> "focus-next-unread"
  NextUnreadUserOrChannelEvent  -> "focus-next-unread-user-or-channel"
  LastChannelEvent          -> "focus-last-channel"

  SelectNextTabEvent        -> "select-next-tab"
  SelectPreviousTabEvent    -> "select-previous-tab"

  ShowAttachmentListEvent   -> "show-attachment-list"

  EnterFlaggedPostsEvent    -> "show-flagged-posts"
  ToggleChannelListVisibleEvent -> "toggle-channel-list-visibility"
  ShowHelpEvent             -> "show-help"
  EnterSelectModeEvent      -> "select-mode"
  EnterOpenURLModeEvent     -> "enter-url-open"

  LoadMoreEvent             -> "load-more"
  OpenMessageURLEvent       -> "open-message-url"

  ScrollUpEvent     -> "scroll-up"
  ScrollDownEvent   -> "scroll-down"
  ScrollLeftEvent   -> "scroll-left"
  ScrollRightEvent  -> "scroll-right"
  PageUpEvent       -> "page-up"
  PageDownEvent     -> "page-down"
  ScrollTopEvent    -> "scroll-top"
  ScrollBottomEvent -> "scroll-bottom"

  SelectUpEvent   -> "select-up"
  SelectDownEvent -> "select-down"

  SearchSelectUpEvent   -> "search-select-up"
  SearchSelectDownEvent -> "search-select-down"

  ActivateListItemEvent -> "activate-list-item"

  FlagMessageEvent   -> "flag-message"
  PinMessageEvent   -> "pin-message"
  ViewMessageEvent   -> "view-message"
  FillGapEvent       -> "fetch-for-gap"
  YankMessageEvent   -> "yank-message"
  YankWholeMessageEvent   -> "yank-whole-message"
  DeleteMessageEvent -> "delete-message"
  EditMessageEvent   -> "edit-message"
  ReplyMessageEvent  -> "reply-message"
  ReactToMessageEvent -> "react-to-message"

  AttachmentListAddEvent    -> "add-to-attachment-list"
  AttachmentListDeleteEvent -> "delete-from-attachment-list"
  AttachmentOpenEvent       -> "open-attachment"
