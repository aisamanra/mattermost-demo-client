{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | This module provides a set of data types to represent message text.
-- The types here are based loosely on the @cheapskate@ package's types
-- but provide higher-level support for the kinds of things we find in
-- Mattermost messages such as user and channel references.
--
-- To parse a Markdown document, use 'parseMarkdown'. To actually render
-- text in this representation, see the module 'Draw.RichText'.
module Matterhorn.Types.RichText
  ( Doc(..)
  , ListType(..)
  , CodeBlockInfo(..)
  , NumDecoration(..)
  , Element(..)
  , TeamBaseURL(..)
  , TeamURLName(..)

  , URL(..)
  , unURL

  , parseMarkdown

  , findUsernames
  , docGetURLs
  -- , findVerbatimChunk

  -- -- Exposed for testing only:
  -- , fromMarkdownBlocks
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Commonmark.Types as C
import qualified Commonmark.Parser as C
import           Data.Char ( isAlphaNum, isAlpha )
import qualified Data.Foldable as F
import qualified Data.Map as M
import           Data.Monoid (First(..))
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import           Data.Sequence ( (<|), ViewL((:<)) )
import qualified Data.Text as T

import           Network.Mattermost.Types ( PostId(..), Id(..), ServerBaseURL(..) )

import           Matterhorn.Constants ( userSigil, normalChannelSigil )

-- | A team name found in a Mattermost post URL
data TeamURLName = TeamURLName Text
                 deriving (Eq, Show, Ord)

-- | A server base URL with a team name.
data TeamBaseURL = TeamBaseURL TeamURLName ServerBaseURL
                 deriving (Eq, Show)

-- | A rich text document.
data Doc =
    EmptyDoc
    -- ^ The empty document
    | Blocks (Seq Doc)
    -- ^ Block sequence
    | Para Element
    -- ^ A paragraph.
    | Header Int Element
    -- ^ A section header with specified depth and contents.
    | Blockquote Doc
    -- ^ A blockquote.
    | List ListType (Seq Doc)
    -- ^ An itemized list.
    | CodeBlock CodeBlockInfo Text
    -- ^ A code block.
    | HTMLBlock Text
    -- ^ A fragment of raw HTML.
    | HRule
    -- ^ A horizontal rule.
    | Heading Int Element
    -- ^ A section heading
    deriving (Show)

-- | The type of itemized list items.
data ListType =
    Bullet Char
    -- ^ Decorate the items with bullet using the specified character.
    | Numbered NumDecoration Int
    -- ^ Number the items starting at the specified number; use the
    -- indicated decoration following the number.
    deriving (Eq, Show, Ord)

-- | Information about a code block.
data CodeBlockInfo =
    CodeBlockInfo { codeBlockLanguage :: Maybe Text
                  -- ^ The language of the source code in the code
                  -- block, if any. This is encoded in Markdown as a
                  -- sequence of non-whitespace characters following the
                  -- fenced code block opening backticks.
                  , codeBlockInfo :: Maybe Text
                  -- ^ Any text that comes after the language token.
                  -- This text is separated from the language token by
                  -- whitespace.
                  }
                  deriving (Eq, Show, Ord)

-- | Ways to decorate numbered itemized list items. The decoration
-- follows the list item number.
data NumDecoration =
    Paren
    | Period
    deriving (Eq, Show, Ord)

-- | A URL.
newtype URL = URL Text
            deriving (Eq, Show, Ord)

unURL :: URL -> Text
unURL (URL url) = url

-- | The kinds of data that can appear in rich text elements.
data Element =
    EEmpty
    -- ^ The empty element
    | ESeq (Seq Element)
    -- ^ Concat
    | EText Text
    -- ^ A sequence of non-whitespace characters.
    | ESpace
    -- ^ A single space.
    | ESoftBreak
    -- ^ A soft line break.
    | ELineBreak
    -- ^ A hard line break.
    | ERaw Text
    -- ^ Raw content.
    | EEditSentinel Bool
    -- ^ A sentinel indicating that some text has been edited (used
    -- to indicate that mattermost messages have been edited by their
    -- authors). This has no parsable representation; it is only used
    -- to annotate a message prior to rendering to add a visual editing
    -- indicator. The boolean indicates whether the edit was "recent"
    -- (True) or not (False).
    | EUser Text
    -- ^ A user reference. The text here includes only the username, not
    -- the sigil.
    | EChannel Text
    -- ^ A channel reference. The text here includes only the channel
    -- name, not the sigil.
    | EHyperlink URL Element
    -- ^ A hyperlink to the specified URL. Optionally provides an
    -- element sequence indicating the URL's text label; if absent, the
    -- label is understood to be the URL itself.
    | EImage URL Element
    -- ^ An image at the specified URL. Optionally provides an element
    -- sequence indicating the image's "alt" text label; if absent, the
    -- label is understood to be the URL itself.
    | EEmoji Text
    -- ^ An emoji reference. The text here includes only the text
    -- portion, not the colons, e.g. "foo" instead of ":foo:".
    | ENonBreaking Element
    -- ^ A sequence of elements that must never be separated during line
    -- wrapping.
    | EPermalink TeamURLName PostId Element
    -- ^ A permalink to the specified team (name) and post ID with an
    -- optional label.
    | EStrong Element
    -- ^ Strong formatting
    | EEmph Element
    -- ^ Emphasis formatting
    | ECode Text
    -- ^ Inline code formatting (TODO: support wrapping these)
    deriving (Show, Eq, Ord)

instance Monoid Element where
    mempty = EEmpty

instance Semigroup Element where
    EEmpty <> b                = b
    a <> EEmpty                = a
    (EStrong a) <> (EStrong b) = EStrong $ a <> b
    (EEmph a) <> (EEmph b)     = EEmph $ a <> b
    (ESeq a) <> (ESeq b)       = ESeq $ a <> b
    (ESeq a) <> b              = ESeq $ a Seq.|> b
    a <> (ESeq b)              = ESeq $ a Seq.<| b
    a <> b                     = ESeq $ Seq.fromList [a, b]

instance C.HasAttributes Element where
    addAttributes _ i = i

instance C.Rangeable Element where
    ranged _ i = i

instance C.IsInline Element where
    lineBreak = ELineBreak
    softBreak = ESoftBreak
    str = EText
    entity = EText
    escapedChar = EText . T.singleton
    emph = EEmph
    strong = EStrong
    link url _title desc = EHyperlink (URL url) desc
    image url _title desc = EImage (URL url) desc
    code = ECode
    rawInline _ = ERaw

instance Monoid Doc where
    mempty = EmptyDoc

instance Semigroup Doc where
    EmptyDoc <> b            = b
    a <> EmptyDoc            = a
    (Blocks a) <> (Blocks b) = Blocks $ a <> b
    (Blocks a) <> b          = Blocks $ a Seq.|> b
    a <> (Blocks b)          = Blocks $ a Seq.<| b
    a <> b                   = Blocks $ Seq.fromList [a, b]

instance C.Rangeable Doc where
    ranged _ b = b

instance C.HasAttributes Doc where
    addAttributes _ b = b

instance C.IsBlock Element Doc where
    paragraph = Para
    plain = Para
    thematicBreak = HRule
    blockQuote = Blockquote
    codeBlock _info content = CodeBlock (CodeBlockInfo Nothing Nothing) content
    heading = Heading
    rawBlock _format content = CodeBlock (CodeBlockInfo Nothing Nothing) content
    list ty _spacing bs = List (fromMarkdownListType ty) $ Seq.fromList bs
    referenceLinkDefinition _label (_dest, _title) = EmptyDoc

parseMarkdown :: T.Text -> Doc
parseMarkdown t =
    case C.commonmark "-" t of
        Left _ -> EmptyDoc
        Right b -> b

-- parseMarkdown :: Maybe TeamBaseURL -> T.Text -> Seq RichTextBlock
-- parseMarkdown baseUrl t =
--     fromMarkdownBlocks baseUrl bs where C.Doc _ bs = C.markdown C.def t
-- 
-- -- | Convert a sequence of markdown (Cheapskate) blocks into rich text
-- -- blocks.
-- fromMarkdownBlocks :: Maybe TeamBaseURL -> C.Blocks -> Seq RichTextBlock
-- fromMarkdownBlocks baseUrl = fmap (fromMarkdownBlock baseUrl)
-- 
-- -- | Convert a single markdown block into a single rich text block.
-- fromMarkdownBlock :: Maybe TeamBaseURL -> C.Block -> RichTextBlock
-- fromMarkdownBlock baseUrl (C.Para is) =
--     Para $ fromMarkdownInlines baseUrl is
-- fromMarkdownBlock baseUrl (C.Header level is) =
--     Header level $ fromMarkdownInlines baseUrl is
-- fromMarkdownBlock baseUrl (C.Blockquote bs) =
--     Blockquote $ fromMarkdownBlock baseUrl <$> bs
-- fromMarkdownBlock baseUrl (C.List f ty bss) =
--     List f (fromMarkdownListType ty) $ fmap (fromMarkdownBlock baseUrl) <$> Seq.fromList bss
-- fromMarkdownBlock _ (C.CodeBlock attr body) =
--     CodeBlock (fromMarkdownCodeAttr attr) body
-- fromMarkdownBlock _ (C.HtmlBlock body) =
--     HTMLBlock body
-- fromMarkdownBlock _ C.HRule =
--     HRule
-- 
-- fromMarkdownCodeAttr :: C.CodeAttr -> CodeBlockInfo
-- fromMarkdownCodeAttr (C.CodeAttr lang info) =
--     let strippedLang = T.strip lang
--         strippedInfo = T.strip info
--         maybeText t = if T.null t then Nothing else Just t
--     in CodeBlockInfo (maybeText strippedLang)
--                      (maybeText strippedInfo)

fromMarkdownListType :: C.ListType -> ListType
fromMarkdownListType (C.BulletList c) =
    Bullet c
fromMarkdownListType (C.OrderedList i _enum delim) =
    let dec = case delim of
                  C.Period -> Period
                  C.OneParen -> Paren
                  C.TwoParens -> Paren
    in Numbered dec i

-- -- | Remove hyperlinks from an inline sequence. This should only be used
-- -- for sequences that are themselves used as labels for hyperlinks; this
-- -- prevents them from embedding their own hyperlinks, which is nonsense.
-- --
-- -- Any hyperlinks found in the sequence will be replaced with a text
-- -- represent of their URL (if they have no label) or the label itself
-- -- otherwise.
-- removeHyperlinks :: Seq C.Inline -> Seq C.Inline
-- removeHyperlinks is =
--     case Seq.viewl is of
--         h :< t ->
--             case h of
--                 C.Link label theUrl _ ->
--                     if Seq.null label
--                     then C.Str theUrl <| removeHyperlinks t
--                     else removeHyperlinks label <> removeHyperlinks t
--                 _ -> h <| removeHyperlinks t
--         Seq.EmptyL -> mempty
-- 
-- -- | Convert a sequence of Markdown inline values to a sequence of
-- -- Elements.
-- --
-- -- This conversion converts sequences of Markdown AST fragments into
-- -- RichText elements. In particular, this function may determine that
-- -- some sequences of Markdown text fragments belong together, such as
-- -- the sequence @["@", "joe", "-", "user"]@, which should be treated as
-- -- a single "@joe-user" token due to username character validation. When
-- -- appropriate, this function does such consolidation when converting to
-- -- RichText elements.
-- --
-- -- This function is also partially responsible for paving the way for
-- -- line-wrapping later on when RichText is rendered. This means that,
-- -- when possible, the elements produced by this function need to be as
-- -- small as possible, without losing structure information. An example
-- -- of this is Markdown inline code fragments, such as "`this is code`".
-- -- This function will convert that one Markdown inline code fragment
-- -- into a sequence of five RichText elements, each with a "Code" style
-- -- assigned: @[EText "this", ESpace, EText "is", ESpace, EText "code"]@.
-- -- This "flattening" of the original Markdown structure makes it much
-- -- easier to do line-wrapping without losing style information because
-- -- it is possible to gather up tokens that do not exceed line widths
-- -- without losing style information. This is key to rendering the text
-- -- with the right terminal attributes.
-- --
-- -- However, there are a couple of cases where such flattening does *not*
-- -- happen: hyperlinks and images. In these cases we do not flatten the
-- -- (arbitrary) text label structure of links and images because we need
-- -- to be able to recover those labels to gather up URLs to show to the
-- -- user in the URL list. So we leave the complete text structure of
-- -- those labels behind in the 'EHyperlink' and 'EImage' constructors as
-- -- sequences of Elements. This means that logic in 'Draw.RichText' that
-- -- does line-wrapping will have to explicitly break up link and image
-- -- labels across line breaks.
-- fromMarkdownInlines :: Maybe TeamBaseURL -> Seq C.Inline -> Seq Element
-- fromMarkdownInlines baseUrl inlines =
--     let go sty is = case Seq.viewl is of
--           C.Str "~" :< xs ->
--               case Seq.viewl xs of
--                   C.Str "~" :< xs2 ->
--                       case takeUntilStrikethroughEnd xs2 of
--                           Nothing -> Element sty (EText "~") <|
--                                      go sty xs
--                           Just (strikethroughInlines, rest) ->
--                               go Strikethrough strikethroughInlines <>
--                               go sty rest
--                   _ ->
--                       let (cFrags, rest) = Seq.spanl isNameFragment xs
--                           cn = T.concat (unsafeGetStr <$> F.toList cFrags)
--                       in if not (T.null cn)
--                          then Element sty (EChannel cn) <| go sty rest
--                          else Element sty (EText normalChannelSigil) <| go sty xs
--           C.Str ":" :< xs ->
--               let validEmojiFragment (C.Str f) =
--                       f `elem` ["_", "-"] || T.all isAlphaNum f
--                   validEmojiFragment _ = False
--                   (emojiFrags, rest) = Seq.spanl validEmojiFragment xs
--                   em = T.concat $ unsafeGetStr <$> F.toList emojiFrags
--               in case Seq.viewl rest of
--                   C.Str ":" :< rest2 ->
--                       Element Normal (EEmoji em) <| go sty rest2
--                   _ ->
--                       Element sty (EText ":") <| go sty xs
--           C.Str t :< xs | userSigil `T.isPrefixOf` t ->
--               let (uFrags, rest) = Seq.spanl isNameFragment xs
--                   t' = T.concat $ t : (unsafeGetStr <$> F.toList uFrags)
--                   u = T.drop 1 t'
--               in Element sty (EUser u) <| go sty rest
--           C.Str t :< xs ->
--               -- When we encounter a string node, we go ahead and
--               -- process the rest of the nodes in the sequence. If the
--               -- new sequence starts with *another* string node with
--               -- the same style, we merge them. We do this because
--               -- Cheapskate will parse things like punctuation as
--               -- individual string nodes, but we want to keep those
--               -- together with any text is adjacent to them to avoid
--               -- e.g. breaking up quotes from quoted text when doing
--               -- line-wrapping. It's important to make sure we only do
--               -- this for adjacent string (i.e. not whitespace) nodes
--               -- and that we make sure we only merge when they have the
--               -- same style.
--               let rest = go sty xs
--                   e = Element sty (EText t)
--               in case Seq.viewl rest of
--                   Element sty2 (EText t2) :< tail_ | sty2 == sty ->
--                       (Element sty (EText (t <> t2))) <| tail_
--                   _ ->
--                       e <| rest
--           C.Space :< xs ->
--               Element sty ESpace <| go sty xs
--           C.SoftBreak :< xs ->
--               Element sty ESoftBreak <| go sty xs
--           C.LineBreak :< xs ->
--               Element sty ELineBreak <| go sty xs
--           C.Link label theUrl _ :< xs ->
--               let mLabel = if Seq.null label
--                            then Nothing
--                            else case F.toList label of
--                                [C.Str u] | u == theUrl -> Nothing
--                                _ -> Just $ fromMarkdownInlines baseUrl $ removeHyperlinks label
--                   rest = go sty xs
--                   this = case flip getPermalink theUrl =<< baseUrl of
--                       Nothing ->
--                           let url = URL theUrl
--                           in Element (Hyperlink url sty) $ EHyperlink url mLabel
--                       Just (tName, pId) ->
--                           Element Permalink $ EPermalink tName pId mLabel
--               in this <| rest
--           C.Image altIs theUrl _ :< xs ->
--               let mLabel = if Seq.null altIs
--                            then Nothing
--                            else Just $ fromMarkdownInlines baseUrl altIs
--                   url = URL theUrl
--               in (Element (Hyperlink url sty) $ EImage url mLabel) <| go sty xs
--           C.RawHtml t :< xs ->
--               Element sty (ERawHtml t) <| go sty xs
--           C.Code t :< xs ->
--               -- We turn a single code string into individual Elements
--               -- so we can perform line-wrapping on the inline code
--               -- text.
--               let ts = [ Element Code frag
--                        | wd <- T.split (== ' ') t
--                        , frag <- case wd of
--                            "" -> [ESpace]
--                            _  -> [ESpace, EText wd]
--                        ]
--                   ts' = case ts of
--                     (Element _ ESpace:rs) -> rs
--                     _                     -> ts
--               in Seq.fromList ts' <> go sty xs
--           C.Emph as :< xs ->
--               go Emph as <> go sty xs
--           C.Strong as :< xs ->
--               go Strong as <> go sty xs
--           C.Entity t :< xs ->
--               Element sty (EText t) <| go sty xs
--           Seq.EmptyL -> mempty
-- 
--     in go Normal inlines
-- 
-- takeUntilStrikethroughEnd :: Seq C.Inline -> Maybe (Seq C.Inline, Seq C.Inline)
-- takeUntilStrikethroughEnd is =
--     let go pos s = case Seq.viewl s of
--             C.Str "~" :< rest ->
--                 case Seq.viewl rest of
--                     C.Str "~" :< _ ->
--                         Just pos
--                     _ -> go (pos + 1) rest
--             _ :< rest -> go (pos + 1) rest
--             Seq.EmptyL -> Nothing
--     in do
--         pos <- go 0 is
--         let (h, t) = Seq.splitAt pos is
--         return (h, Seq.drop 2 t)
-- 
-- -- | If the specified URL matches the active server base URL and team
-- -- and refers to a post, extract the team name and post ID values and
-- -- return them.
-- getPermalink :: TeamBaseURL -> Text -> Maybe (TeamURLName, PostId)
-- getPermalink (TeamBaseURL tName (ServerBaseURL baseUrl)) url =
--     let newBaseUrl = if "/" `T.isSuffixOf` baseUrl
--                      then baseUrl
--                      else baseUrl <> "/"
--     in if not $ newBaseUrl `T.isPrefixOf` url
--        then Nothing
--        else let rest = T.drop (T.length newBaseUrl) url
--                 (tName', rawPIdStr) = T.breakOn "/pl/" rest
--                 pIdStr = T.drop 4 rawPIdStr
--             in if tName == TeamURLName tName' && not (T.null pIdStr)
--                then Just (tName, PI $ Id pIdStr)
--                else Nothing
-- 
-- 
-- unsafeGetStr :: C.Inline -> Text
-- unsafeGetStr (C.Str t) = t
-- unsafeGetStr _ = error "BUG: unsafeGetStr called on non-Str Inline"

-- | Obtain all username references in a sequence of rich text blocks.
findUsernames :: Doc -> S.Set T.Text
findUsernames = docFindUsernames

docFindUsernames :: Doc -> S.Set T.Text
docFindUsernames (Para e) =
    elementFindUsernames e
docFindUsernames (Header _ e) =
    elementFindUsernames e
docFindUsernames (Blockquote bs) =
    findUsernames bs
docFindUsernames (List _ bs) =
    S.unions $ F.toList $ findUsernames <$> bs
docFindUsernames _ =
    mempty

elementFindUsernames :: Element -> S.Set T.Text
elementFindUsernames el =
    case el of
          EUser u -> S.singleton u
          EHyperlink _ e -> elementFindUsernames e
          EImage _ e -> elementFindUsernames e
          ENonBreaking e -> elementFindUsernames e
          EStrong e -> elementFindUsernames e
          EEmph e -> elementFindUsernames e
          ESeq es -> S.unions $ F.toList $ elementFindUsernames <$> es
          EPermalink _ _ e -> elementFindUsernames e
          EEmoji _ -> mempty
          ECode _ -> mempty
          EEmpty -> mempty
          EText _ -> mempty
          ESpace -> mempty
          ESoftBreak -> mempty
          ELineBreak -> mempty
          ERaw _ -> mempty
          EEditSentinel _ -> mempty
          EChannel _ -> mempty

-- | Obtain all URLs (and optional labels) in a rich text block.
docGetURLs :: Doc -> [(Either (TeamURLName, PostId) URL, Element)]
docGetURLs (Para e) =
    elementGetURLs e
docGetURLs (Header _ e) =
    elementGetURLs e
docGetURLs (Blockquote d) =
    docGetURLs d
docGetURLs (List _ bs) =
    mconcat $ F.toList $ docGetURLs <$> bs
docGetURLs (Blocks bs) =
    mconcat $ F.toList $ docGetURLs <$> bs
docGetURLs _ =
    mempty

elementGetURLs :: Element -> [(Either (TeamURLName, PostId) URL, Element)]
elementGetURLs (EHyperlink url label) =
    [(Right url, label)]
elementGetURLs (EImage url label) =
    [(Right url, label)]
elementGetURLs (EPermalink tName pId label) =
    [(Left (tName, pId), label)]
elementGetURLs (ESeq es) =
    concat $ elementGetURLs <$> (F.toList es)
elementGetURLs (ENonBreaking e) =
    elementGetURLs e
elementGetURLs (EStrong e) =
    elementGetURLs e
elementGetURLs (EEmph e) =
    elementGetURLs e
elementGetURLs EEmpty = []
elementGetURLs (EText _) = []
elementGetURLs ESpace = []
elementGetURLs ESoftBreak = []
elementGetURLs ELineBreak = []
elementGetURLs (ERaw _) = []
elementGetURLs (EEditSentinel _) = []
elementGetURLs (EUser _) = []
elementGetURLs (EChannel _) = []
elementGetURLs (EEmoji _) = []
elementGetURLs (ECode _) = []

-- -- | Find the first code block in a sequence of rich text blocks.
-- findVerbatimChunk :: Seq RichTextBlock -> Maybe Text
-- findVerbatimChunk = getFirst . F.foldMap go
--   where go (CodeBlock _ t) = First (Just t)
--         go _               = First Nothing
-- 
-- isValidNameChar :: Char -> Bool
-- isValidNameChar c = isAlpha c || c == '_' || c == '.' || c == '-'
-- 
-- isNameFragment :: C.Inline -> Bool
-- isNameFragment (C.Str t) =
--     not (T.null t) && isValidNameChar (T.head t)
-- isNameFragment _ = False
