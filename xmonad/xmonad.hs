{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Arrow
  ( first
  , second
  )
import Control.Monad
  ( (<=<)
  , foldM
  , when
  )
import Data.Bits
  ( (.&.) )
import Data.List
  ( elemIndex
  , find
  , foldl'
  , intercalate
  , isPrefixOf
  , nub
  , sortBy
  , sortOn
  )
import Data.Maybe
  ( fromJust
  , isJust
  , isNothing
  , listToMaybe
  )
import Data.Monoid
  ( All(..)
  , appEndo
  )
import Data.Ord
  ( comparing
  )
import Graphics.X11.Types
import Safe
  ( headDef )
import System.Posix.Unistd
  ( nodeName
  , getSystemID
  )
import XMonad
import XMonad.Actions.CycleWS
  ( Direction1D
    ( Next
    , Prev
    )
  , WSType (WSIs)
  , doTo
  , findWorkspace
  , moveTo
  , screenBy
  )
import XMonad.Actions.GridSelect
  ( gridselect
  , spawnSelected
  , stringColorizer
  , buildDefaultGSConfig
  , gs_cellheight
  , gs_cellwidth
  )
import XMonad.Actions.NoBorders
  ( toggleBorder )
import XMonad.Actions.Warp
  ( banish
  , Corner(..))
import XMonad.Hooks.ManageHelpers
  ( (-?>)
  , doCenterFloat
  , composeOne
  )
import XMonad.Layout.IndependentScreens
  ( countScreens )
import XMonad.Layout.NoBorders
  ( WithBorder
  , noBorders
  )
import XMonad.Layout.ResizableTile
  ( MirrorResize
    ( MirrorExpand
    , MirrorShrink
    )
  , ResizableTall(..)
  )
import XMonad.Util.Font
  ( Align(..)
  , fi
  , initXMF
  , releaseXMF
  , textExtentsXMF
  , textWidthXMF
  )
import XMonad.Util.NamedWindows
  ( getName )
import XMonad.Util.Timer
  ( TimerId
  , handleTimer
  , startTimer
  )
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceCompare
  ( getSortByIndex )
import XMonad.Util.XUtils
  ( createNewWindow
  , deleteWindow
  , paintAndWrite
  , showWindow
  )
import qualified Data.Map as M
import qualified Data.Map.Strict as H
import qualified XMonad.Layout.LayoutModifier as XLL
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

-- The main concept behind this configuration is the 3-dimensional navigation of
-- Workspaces. To my knowledge it is unique in the XMonad world. It builds on a
-- simple idea from XMonad.Layout.IndependentScreens (the idea of adding a
-- dimension by merely modifying a WorkspaceId (string)). A summary of how that
-- works follows below. The other thing is that all toplevel functions in here
-- are prefixed with "l_", in the style of my Emacs configuration (where all
-- custom functions are prefixed with "l/").

newtype XCoord = X Int
  deriving (Eq, Ord)
newtype ZCoord = Z Int
  deriving (Eq, Ord)
newtype YCoord = Y Int
  deriving (Eq, Ord)

-- Each WorkspaceId, aka Workspace name, is the unique cross-section of the X,
-- Z, and Y axes. The WorkspaceId is a String, and we can convert a XZY to a
-- String with a basic "Show" instance.
newtype XZY = XZY (XCoord, ZCoord, YCoord)
  deriving (Eq, Ord)
instance Show XZY where
  show (XZY (X x, Z z, Y y)) = intercalate "_" $ map show [x, z, y]

-- XMonad internally calls the WorkspaceId a "tag" as it is just a String
-- (WorkspaceId is type-synonymed to it). We use a combination of X, Y, and
-- Z-axis coordinates to get a 3-dimensional view of Workspaces. We do this by
-- breaking up WorkspaceId into the following format:
-- "<XCoord>_<ZCoord>_<YCoord>".
--
-- The H-h/H-l bindings move the active workspace (or window) focus across
-- physical screens in the X-axis direction (the vanilla XMonad bindings are
-- Modkey-{Q,W,E}). We chose the name X-axis for this direction because on most
-- multihead setups, the monitors are placed horizontally in a
-- left-to-right fashion. To move into our Z axis, we use H-n; this binding
-- switches the workspace on the current physical screen only and leaves all
-- other screens as-is. Intuitively, we can imagine this 2-dimensional scheme as
-- if each XMonad worspace is a playing card in a deck of cards. Each physical
-- screen has its own unique deck. Not to be pedantic, but for those unfamiliar
-- with XMonad, each "playing card" here is its own "Desktop" with multiple
-- tiled GUI windows of applications.
--
-- Now we add in our final dimension, the Y dimension. The H-M-j/H-M-k bindings
-- move up and down the Y axis. Continuing with our playing card analogy, it's
-- as if each <YCoord> in the Y axis has its own independent array of decks. The
-- most interesting thing here is that moving up and down the Y axis changes
-- _all_ screens (every deck must change!). The effect is quite dramatic (and
-- refreshing on my 4-monitor setup) and really shows off the power of
-- multi-head setups. This simulates the workflow demonstrated in
-- https://youtu.be/w5_36BBGoU0?t=3m30s, where multiple screens all change
-- content at once. The cool part about our setup is that we have the added Z
-- dimension (for each unique X and Y coordinate pair), giving us much more room
-- to place windows around. An important thing to keep in mind here is that even
-- though we change by the Y coordinate with H-M-j/H-M-k, we actually change the
-- Z coordinate as well if necessary on a per-screen basis (for each physical
-- screen (XCoord)). This is because whenever we move up/down the Y axis, we
-- move to the *last used set of XZY workspaces at that YCoord*; using our
-- playing card example, each Y coordinate (array of decks) remembers what
-- ZCoord (playing card) each X screen (deck) was showing before focus changed
-- to a different Y coordinate. This "last seen" concept is made possible with
-- the "Seen" data type. We also save which screen had focus, which comes in
-- handy when switching rapidly between Y coordinates.
--
-- That's about it. Maybe one day this will become an XMonad extension...

-- The number of connected physical screens can vary across OS boots (e.g., when
-- we disconnect or connect an external monitor to a laptop). So we rely on
-- XMonad.Layout.IndependentScreens to give us the correct number of X
-- coordinates (see countScreens), and use it here as numScreens. (NOTE: it
-- is unclear what will happen if we change the number of physical screens
-- within a single X Session.)
l_XCoords :: Int -> [XCoord]
l_XCoords numScreens = map X $ take numScreens [0..]

-- We have 10 Z coordinates and 10 Y coordinates. That means there are 100
-- unique workspaces, which is more than enough, one would think, for most work.
--
-- The X coordinate is composed of the number of monitor screens available in
-- the real world.
l_ZCoords :: [ZCoord]
l_ZCoords = map Z $ take 10 [0..]
l_YCoords :: [YCoord]
l_YCoords = map Y $ take 10 [0..]

-- Again, for reference our format for WorkspaceIds are "<x>_<z>_<y>".
l_XFrom :: XZY -> XCoord
l_XFrom (XZY (xc, _, _))= xc
l_ZFrom :: XZY -> ZCoord
l_ZFrom (XZY (_, zc, _))= zc
l_YFrom :: XZY -> YCoord
l_YFrom (XZY (_, _, yc))= yc

l_XFromWid :: WorkspaceId -> XCoord
l_XFromWid wid = X $ read xString
  where
  xString = takeWhile (/='_') wid
l_ZFromWid :: WorkspaceId -> ZCoord
l_ZFromWid wid = Z $ read zString
  where
  zString = takeWhile (/='_') . drop 1 . snd $ break (=='_') wid
l_YFromWid :: WorkspaceId -> YCoord
l_YFromWid wid = Y $ read yString
  where
  yString = drop 1 $ dropWhile (/='_') $ drop 1 $ dropWhile (/='_') wid

-- Group ZCoords by type. Usinge our playing card analogy, these groups are like
-- the "suits" of cards.
data ZGroup
  = ZGWork
  | ZGNet
  | ZGMisc
  | ZGSys
  deriving (Eq, Ord, Enum, Show)

l_ZCoordToGroup :: ZCoord -> ZGroup
l_ZCoordToGroup (Z z)
  | elem z [0..2] = ZGWork
  | elem z [3..4] = ZGNet
  | z == 5 = ZGSys
  | otherwise = ZGMisc

l_ZGroupToZCoords :: ZGroup -> [ZCoord]
l_ZGroupToZCoords zGroup
  = map fst
  $ filter ((== zGroup) . snd) l_ZCoordGroups

l_ZCoordGroups :: [(ZCoord, ZGroup)]
l_ZCoordGroups =
  [ (z, l_ZCoordToGroup z)
  | z <- l_ZCoords
  ]

-- Generate all possible XZYs. Sort by the Y, X, and Z coordinates, in that
-- order. This is important because, for instance, XMonad assigns each physical
-- screen to each WorkspaceId in this list when we use it in assigning
-- workspaces.
l_XZYs :: Int -> [XZY]
l_XZYs numScreens = l_multiSort
  [ XZY (x, z, y)
  | x <- l_XCoords numScreens
  , z <- l_ZCoords
  , y <- l_YCoords
  ]
  [ comparing l_ZFrom
  , comparing l_YFrom
  ]

-- Each time we change the Y coordinate, we record the ZCoord on each XCoord.
-- For simplicity, instead of storing the ZCoord and XCoord pair, we instead
-- store the entire XZY string (which is also the full, unique WorkspaceId). We
-- use XMonad.Util.ExtensibleState to store this state. See
-- https://stackoverflow.com/questions/40270793/user-state-in-xmonad
data Seen = Seen (H.Map YCoord (XZY, [XZY])) [YCoord]
instance ExtensionClass Seen where
  initialValue = Seen H.empty []

l_YFromWindowSet :: WindowSet -> YCoord
l_YFromWindowSet = l_YFromWid . W.tag . W.workspace . W.current

l_XZYFromWindowSet :: WindowSet -> XZY
l_XZYFromWindowSet windowSet
  = XZY
  . l_CoordsFromWid
  . W.tag . W.workspace $ W.current windowSet

l_XZYFromWid :: WorkspaceId -> XZY
l_XZYFromWid = XZY . l_CoordsFromWid

l_CoordsFromWid :: WorkspaceId -> (XCoord, ZCoord, YCoord)
l_CoordsFromWid xzy =
  ( l_XFromWid xzy
  , l_ZFromWid xzy
  , l_YFromWid xzy
  )

l_YIncrementedBy :: Direction1D -> Bool -> YCoord -> YCoord
l_YIncrementedBy dir safe (Y y) = if safe
  then Y $ mod (op y 1) (length l_YCoords)
  else Y $ op y 1
  where
  op = if dir == Next then (+) else (-)

-- Given the X, Y, and ZGroup constraints, generate the full XZY coordinate
-- WorkspaceId by deciding on the ZCoord.
l_XZYFrom :: XCoord -> Int -> ZGroup -> YCoord -> XZY
l_XZYFrom (X xCoord) numScreens zGroup y = XZY (x, z, y)
  where
  -- Wrap xCoord if it is out of bounds.
  x = l_XCoords numScreens !! mod xCoord numScreens
  -- We just grab the very first ZCoord in the ZGroup.
  z = head $ l_ZGroupToZCoords zGroup

-- For a given ZGroup and YCoord, get a list of XZYs belonging to that ZGroup
-- for every XCoord. The pool of available ZCoords is modified once by
-- transformZCoords.
l_XZYsFrom :: Int -> ZGroup -> ([ZCoord] -> [ZCoord]) -> YCoord -> [XZY]
l_XZYsFrom numScreens zGroup transformZCoords y =
  [ XZY (x, z, y)
  | x <- l_XCoords numScreens
  , z <- transformZCoords $ l_ZGroupToZCoords zGroup
  ]

-- Abstraction to help us search for a Workspace. See l_searchZ for more info.
data WorkspaceQuery = WQ WindowPopulation ZGroupMemberships
data WindowPopulation
  = Empty
  | NonEmpty
  deriving (Eq)

-- The boolean in the tuple determines whether we should check if the XZY in the
-- Workspace belongs to the ZGroup (True for yes, it belongs; False for no it
-- does not belong).
type ZGroupMemberships = [(ZGroup, Bool)]

l_if :: X Bool -> X () -> X () -> X ()
l_if p a b = p >>= \pb -> if pb then a else b

newtype FadingPrompts = FadingPrompts [(TimerId, Window)] deriving (Eq, Typeable)
instance ExtensionClass FadingPrompts where
  initialValue = FadingPrompts []

data RichText = RichText
  { rtString :: String
  , rtFont :: String
  , rtSize :: Int
  , rtForeground :: String
  , rtBackground :: String
  , rtBorder :: String
  , rtBorderWidth :: Int
  }

richTextDef :: RichText
richTextDef = RichText
  { rtString = " "
  , rtFont = "DejaVu Sans Mono"
  , rtSize = 160
  , rtForeground = "white"
  , rtBackground = "blue"
  , rtBorder = "white"
  , rtBorderWidth = 4
  }

l_showYCoord :: X ()
l_showYCoord = do
  (Y y) <- gets (l_YFromWindowSet . windowset)
  l_displayString $ richTextDef
    { rtString = show y
    , rtForeground = "black"
    , rtBackground = c y
    , rtBorder = "white"
    }
  where
  colors =
    [ "green"
    , "yellow"
    , "blue"
    , "red"
    , "magenta"
    , "cyan"
    , "olive"
    , "light slate gray"
    , "light pink"
    , "white"
    ]
  c n' = colors !! mod n' (length colors)

-- Show the number of hidden *and* nonempty workspaces in the current Z
-- coordinate, if there are any. This reminds the user that even if they think
-- they have deleted all windows, that there are still some workspaces with
-- pending work in them.
l_maybeShowHiddenNonEmptyZCount :: X ()
l_maybeShowHiddenNonEmptyZCount = do
  windowSet <- gets windowset
  numHidden <- l_countHiddenNonEmptyZ
  let
    xzy = l_XZYFromWindowSet windowSet
  noWindows <- l_workspaceIsEmpty xzy
  when (noWindows || (numHidden > 0)) $ l_displayStringDef $ show numHidden

l_countHiddenNonEmptyZ :: X Int
l_countHiddenNonEmptyZ = do
  windowSet <- gets windowset
  let
    xzy = l_XZYFromWindowSet windowSet
    x = l_XFrom xzy
    y = l_YFrom xzy
  pure $ length
    [ ww
    | ww <- W.workspaces windowSet
    -- Don't count current workspace.
    , W.tag ww /= show xzy
    , atXY x y . l_XZYFromWid $ W.tag ww
    , isJust $ W.stack ww
    ]
  where
  atXY x y (XZY (x', _, y')) = x == x' && y == y'

l_displayString :: RichText -> X ()
l_displayString RichText{..} = do
  scr <- gets $ screenRect . W.screenDetail . W.current . windowset
  dpy <- asks display
  f <- initXMF ("xft:" <> rtFont <> ":pixelsize=" <> show rtSize)
  width <- (\w -> w + w `div` length rtString) <$> textWidthXMF dpy f rtString
  (ascent, descent) <- textExtentsXMF f rtString
  let
    height = ascent + descent + 10
    y' = fi (rect_y scr) + div (fi (rect_height scr) - height) 2
    x  = fi (rect_x scr) + div (fi (rect_width scr) - width) 2
  maybeFocusedWindow <- gets $ W.stack . W.workspace . W.current . windowset
  (xFinal, yFinal) <- case maybeFocusedWindow of
    Just s -> do
      (_, x2, y2, w2, h2, _, _) <- liftIO . getGeometry dpy $ W.focus s
      return
        ( x2 + (div (fi w2) 2 - div (fi width) 2)
        , y2 + (div (fi h2) 2 - div height 2)
        )
    Nothing -> return (fi x, y')
  w <- createNewWindow
    (Rectangle (fi xFinal) (fi yFinal) (fi width) (fi height)) Nothing "" True
  showWindow w
  paintAndWrite
    w
    f
    (fi width)
    (fi height)
    (fromIntegral rtBorderWidth)
    rtBackground
    rtBorder
    rtForeground
    rtBackground
    [AlignCenter]
    [rtString]
  releaseXMF f
  -- Show the window for 0.4 seconds.
  tid <- startTimer 0.4
  (FadingPrompts tws) <- XS.get :: X FadingPrompts
  XS.put . FadingPrompts $ (tid, w):tws

l_displayStringOn :: RichText -> WorkspaceId -> X ()
l_displayStringOn RichText{..} wid = do
  windowSet <- gets windowset
  let
    xzy = l_XZYFromWid wid
    scn = head
      [ ss
      | ss <- W.screens windowSet
      , W.tag (W.workspace ss) == show xzy
      ]
    scr = screenRect $ W.screenDetail scn
  dpy <- asks display
  f <- initXMF ("xft:" <> rtFont <> ":pixelsize=" <> show rtSize)
  width <- (\w -> w + w `div` length rtString) <$> textWidthXMF dpy f rtString
  (ascent, descent) <- textExtentsXMF f rtString
  let
    height = ascent + descent + 10
    y' = fi (rect_y scr) + div (fi (rect_height scr) - height) 2
    x  = fi (rect_x scr) + div (fi (rect_width scr) - width) 2
    maybeFocusedWindow = W.stack $ W.workspace scn
  (xFinal, yFinal) <- case maybeFocusedWindow of
    Just s -> do
      (_, x2, y2, w2, h2, _, _) <- liftIO . getGeometry dpy $ W.focus s
      return
        ( x2 + (div (fi w2) 2 - div (fi width) 2)
        , y2 + (div (fi h2) 2 - div height 2)
        )
    Nothing -> return (fi x, y')
  w <- createNewWindow
    (Rectangle (fi xFinal) (fi yFinal) (fi width) (fi height)) Nothing "" True
  showWindow w
  paintAndWrite
    w
    f
    (fi width)
    (fi height)
    (fromIntegral rtBorderWidth)
    rtBackground
    rtBorder
    rtForeground
    rtBackground
    [AlignCenter]
    [rtString]
  releaseXMF f
  -- Show the window for 0.4 seconds.
  tid <- startTimer 0.4
  (FadingPrompts tws) <- XS.get :: X FadingPrompts
  XS.put . FadingPrompts $ (tid, w):tws

l_displayStringDef :: String -> X ()
l_displayStringDef s = l_displayString $ richTextDef
  { rtString = s
  }

l_eventHook :: Event -> X All
l_eventHook e = do
  (FadingPrompts tws) <- XS.get :: X FadingPrompts
  when (not $ null tws) $ do
    let
      f (tid, w) = handleTimer tid e $ do
        deleteWindow w
        return Nothing
    -- We need to process the window deletion across *all* outstanding
    -- FadingPrompts because it may be the case that we receive the events out
    -- of order (that is, although we always build up the events by cons-ing it,
    -- this hook uses XS (global state) to retrieve all known tid values and
    -- runs the window deletion routine against all of them). This brute force
    -- approach is not too terrible, because the deletion will fail if the tid
    -- does not line up with the one in the actual event.
    --
    -- The naive way (which works, but is not guaranteed to work), simply treats
    -- FadingPrompts as a queue and retrieves the last item of the list, then
    -- deletes it from the list after processing.
    mapM_ f tws
  return $ All True

-- This function was written to implement the idea expressed in
-- https://www.reddit.com/r/xmonad/comments/7mawso/switch_workspaces_on_multiple_monitors_with_1/.
-- It involves less work for the user than XMonad.Actions.DynamicWorkspaceGroups
-- because we don't have to manually name workspace groups (basically for us,
-- every YCoord in l_YCoords is a "WorkspaceGroup").
l_viewY :: YCoord -> Bool -> X ()
l_viewY yNext keepXCoord = do
  windowSet <- gets windowset
  when (yNext /= l_YFromWid (W.currentTag windowSet)) $ do
    -- Save xzys of yPrev, so that if and when we switch back to it, we get back
    -- the same workspaces (and not just some random default set of XZYs).
    l_recordXZYs
    -- Activate next YCoord's workspaces. After this operation, visually all
    -- screens will have switched their XZY coordinate to reflect yNext, not
    -- yPrev.
    l_activateY yNext keepXCoord
    l_showYCoord

l_viewYDir :: Direction1D -> Bool -> X ()
l_viewYDir dir keepXCoord = do
  windowSet <- gets windowset
  let
    (XZY (_, _, yPrev)) = l_XZYFromWindowSet windowSet
    yNext = l_YIncrementedBy dir True yPrev
  l_viewY yNext keepXCoord

l_atYEdge :: Direction1D -> X Bool
l_atYEdge dir = do
  windowSet <- gets windowset
  let
    (XZY (_, _, yPrev)) = l_XZYFromWindowSet windowSet
    yNext = l_YIncrementedBy dir False yPrev
  return $ notElem yNext l_YCoords

-- Like l_viewYDir, but first search in the given direction if there are any
-- non-empty XZYs, and if so, view that YCoord. If all other YCoords are empty,
-- do nothing. Note that a YCoord could be non-empty, but that it currently is
-- viewing only empty workspaces; such a YCoord is still considered non-empty.
l_viewYNonEmpty :: Direction1D -> X ()
l_viewYNonEmpty dir = do
  (Seen hashmap _) <- XS.get :: X Seen
  windowSet <- gets windowset
  let
    xzyPrev = l_XZYFromWindowSet windowSet
    yPrev = l_YFrom xzyPrev
    searchDirection = if dir == Next then id else reverse
    -- We say "were" instead of "are", because we use the history stored in Seen
    -- (it is, technically, old information). It could be incorrect (such as
    -- when XMonad is restarted and Seen state is lost --- even though windows
    -- remain populated at various Workspaces).
    xzysWerePopulatedAtY y = (,) y $ case H.lookup y hashmap of
      Just (_, xzys') -> not $ null
        [ xzy
        | ww <- W.workspaces windowSet
        , isJust $ W.stack ww
        , xzy <- xzys'
        , W.tag ww == show xzy
        ]
      Nothing -> False
    rejectWraparound = takeWhile (if dir == Next then (>yPrev) else (<yPrev))
    yNexts
      = map fst
      . dropWhile ((==False) . snd)
      . map xzysWerePopulatedAtY
      . rejectWraparound
      . searchDirection
      $ l_wrapWithout (==yPrev) l_YCoords
  when (not $ null yNexts) $ do
    l_recordXZYs
    l_activateY (head yNexts) False
  l_showYCoord

l_viewNthY :: Int -> Bool -> X ()
l_viewNthY n keepXCoord = do
  (Seen _ ys) <- XS.get :: X Seen
  when (n < length ys) $ do
    let
      y = ys!!n
    l_viewY y keepXCoord
  l_showYCoord

l_recordXZYs :: X ()
l_recordXZYs = do
  windowSet <- gets windowset
  let
    -- Get current (soon to be previous) YCoord.
    y = l_YFrom xzy
    xzy = l_XZYFromWindowSet windowSet
    -- Make note of all XZYs at the current YCoord.
    xzys = map (l_XZYFromWid . W.tag . W.workspace)
      $ W.screens windowSet
    f xzy' = do
      isEmpty <- l_workspaceIsEmpty xzy'
      return (not isEmpty, xzy')
  xzyCurrentIsEmpty <- l_workspaceIsEmpty xzy
  xzyCandidates <- mapM f xzys
  let
    xzyCs = map snd . sortOn snd $ filter fst xzyCandidates
    xzyC
      | xzyCurrentIsEmpty && not (null xzyCs) = head xzyCs
      | otherwise = xzy
  XS.modify
    (\(Seen hashmap ys) -> Seen
      (H.insert y (xzyC, xzys) hashmap)
      ys)

l_updateYHist :: X ()
l_updateYHist = do
  windowSet <- gets windowset
  let
    y = l_YFrom xzy
    xzy = l_XZYFromWindowSet windowSet
  XS.modify
    (\(Seen hm ys) -> Seen
      hm
      (case elemIndex y ys of
        Just i -> l_bubbleUp i ys
        Nothing -> y:ys))

-- Bring the element at the i-th index to the head of the list.
l_bubbleUp :: Int -> [a] -> [a]
l_bubbleUp i xs
  | i < length xs = (xs!!i):(take i xs ++ drop (i + 1) xs)
  | otherwise = xs

-- Make the list cyclic so that we start first with the item that satisifies p,
-- then remove this item.
--
-- Examples:
--
--  l_wrapWithout (==3) [0..5] => [4, 5, 0, 1, 2]
--  l_wrapWithout (==4) [0..5] => [5, 0, 1, 2, 3]
l_wrapWithout :: (a -> Bool) -> [a] -> [a]
l_wrapWithout p xs
  | null xs = xs
  | otherwise = drop 1 after ++ before
  where
  (before, after) = break p xs

-- Make the given YCoord "active" by viewing its XZYs on all physical screen(s).
-- If we viewed the YCoord before, present its XZYs that we recorded when we
-- switched away from it the last time around. Otherwise, show the default XZYs
-- for the YCoord.
l_activateY :: YCoord -> Bool -> X ()
l_activateY y keepXCoord = do
  windowSet <- gets windowset
  (Seen hashmap _) <- XS.get :: X Seen
  let
    numScreens = length $ W.screens windowSet
    (wid, xzys) = case H.lookup y hashmap of
      Just found -> first show found
      Nothing -> ("0", l_defaultXZYsForY y numScreens)
  -- First update all screens.
  windows $ l_viewXZYs xzys
  when (not keepXCoord)
    (windows $ W.view wid)
  l_updateYHist

-- Given a list of XZYs to view, convert each XZY to a "Workspace i l a" type
-- (this is the type that XMonad cares about). In this conversion process, we
-- only look at the `hidden' part of the StackSet (the idea is that we only
-- really want to use this function to view workspaces on a YCoord other than
-- the current one, which means that by definition they should be hidden).
--
-- First we sanitize the input XZYs by making sure that each XZY is (1) unique,
-- (2) has a different XCoord, and (3) can be found in the `hidden' Workspaces
-- list. We then make each XZY visible with `l_promoteFromHidden'.
l_viewXZYs :: [XZY]
  -> WindowSet
  -> WindowSet
l_viewXZYs xzyCandidates windowSet
  -- If none of the xzyCandidates survived the sanity check, then do nothing.
  | null xzys = windowSet
  | otherwise = foldl' l_promoteFromHidden windowSet xzys
  where
  xzys
    = concatMap (flip xzyToWorkspace $ W.hidden windowSet)
    $ nub xzyCandidates
  xzyToWorkspace xzy hiddenWorkspaces =
    [ hiddenWorkspace
    | hiddenWorkspace@(W.Workspace xzy' _ _) <- hiddenWorkspaces
    , xzy' == show xzy
    ]

-- Promote a hidden workspace to be visible. As a reminder, XMonad.Core
-- (confusingly) uses the type synonym WindowSpace to mean "Workspace i l a". We
-- call a WindowSpace "ww" to make the code a little easier to read (better than
-- using "workspace", considering how we already use XMonad.StackSet.workspace
-- as W.workspace).
l_promoteFromHidden :: WindowSet -> WindowSpace -> WindowSet
l_promoteFromHidden windowSet ww
  -- If the given XCoord matches the XCoord of the current (focused) screen, we
  -- have to promote ww to be W.current and demote the existing W.current screen
  -- to be hidden.
  | matchesXCoordFromWW . W.screen $ W.current windowSet = windowSet
    { W.current = (W.current windowSet) { W.workspace = ww }
    , W.hidden = W.workspace (W.current windowSet) : otherHidden
    }
  -- If the workspace we want to see is for a visible screen (not focused), we
  -- have to promote ww to be inside W.visible and demote the existing matching
  -- workspace in W.visible to be hidden.
  | (Just sc) <- find (matchesXCoordFromWW . W.screen) (W.visible windowSet) = windowSet
    { W.visible
      = sc { W.workspace = ww }
      : filter (not . matchesXCoordFromWW . W.screen) (W.visible windowSet)
    , W.hidden = W.workspace sc : otherHidden
    }
  | otherwise = windowSet
  where
  matchesXCoordFromWW (S s) = s == ((\(X x) -> x) . l_XFromWid $ W.tag ww)
  otherHidden = filter ((/= W.tag ww) . W.tag) $ W.hidden windowSet

-- Pick out a default set of workspaces at a particular level. We make sure to
-- pick 1 workspace for each physical screen (given by numScreens).
l_defaultXZYsForY :: YCoord -> Int -> [XZY]
l_defaultXZYsForY y numScreens
  = selectForEachXCoord (l_XCoords numScreens)
  . filter xzyIsOnYCoord
  $ l_XZYs numScreens
  where
  xzyIsOnYCoord (XZY (_, _, y')) = y' == y
  selectForEachXCoord xCoords xzys =
    [ xzy
    | xzy <- xzys
    , elem (l_XFrom xzy) xCoords
    -- Select the xzy at the first ZCoord.
    , l_ZFrom xzy == head l_ZCoords
    ]

-- Move currently selected window to the given YCoord, preserving its XCoord.
l_shiftY :: YCoord -> X ()
l_shiftY yNext = do
  windowSet <- gets windowset
  (Seen hashmap _) <- XS.get :: X Seen
  let
    numScreens = length $ W.screens windowSet
    x = l_XFromWid . W.tag . W.workspace $ W.current windowSet
    -- Like in l_viewYDir, try to grab the XZY of the last used Workspace at the
    -- target xzy.
    xzys = case H.lookup yNext hashmap of
      -- Grab the xzy with the same XCoord as the current screen.
      Just (_, xzys') -> xzys'
      Nothing -> l_defaultXZYsForY yNext numScreens
  whenJust
    (find ((==x) . l_XFrom) xzys)
    (\xzy -> (windows . W.shift $ show xzy) >> l_viewY yNext True)

-- Move currently focused window over to the next YCoord. Then view that YCoord.
-- If there is no currently focused window, don't do anything. This is the
-- Y-axis analogue of the H-S-{h,l} bindings powered by l_shiftAndView.
l_shiftYDir :: Direction1D -> X ()
l_shiftYDir dir = do
  windowSet <- gets windowset
  (Seen hashmap _) <- XS.get :: X Seen
  let
    numScreens = length $ W.screens windowSet
    yPrev = l_YFromWindowSet windowSet
    yNext = l_YIncrementedBy dir True yPrev
    x = l_XFromWid . W.tag . W.workspace $ W.current windowSet
    -- Like in l_viewYDir, try to grab the XZY of the last used Workspace at the
    -- target xzy.
    xzys = case H.lookup yNext hashmap of
      -- Grab the xzy with the same XCoord as the current screen.
      Just (_, xzys') -> xzys'
      Nothing -> l_defaultXZYsForY yNext numScreens
  whenJust
    (find ((==x) . l_XFrom) xzys)
    (\xzy -> (windows . W.shift $ show xzy) >> l_viewYDir dir True)

-- Show all windows at the current YCoord; if selected, then view that window
-- and switch focus to it.
l_gridSelectWithinY :: X ()
l_gridSelectWithinY = do
  windowSet <- gets windowset
  let
    y = l_YFromWindowSet windowSet
    numScreens = length $ W.screens windowSet
    attachName (ww, a) = do
      b <- show <$> getName a
      return (b, (ww, a))
  windowsAtY <- mapM attachName
    $ concat
      [ map (ww,) $ W.integrate' $ W.stack ww
      | ww <- W.workspaces windowSet
      , xzy <- l_XZYs numScreens
      , W.tag ww == show xzy
      , l_YFrom xzy == y
      ]
  selected <- gridselect (gsconfig2 colorizeByXCoord) windowsAtY
  whenJust selected
    (windows . l_viewByGoing . first W.tag)
  where
  colorizeByXCoord (ww, _)
    = stringColorizer
    . (\(X x) -> show x)
    . l_XFromWid
    $ W.tag ww
  gsconfig2 colorizer = (buildDefaultGSConfig colorizer)
    { gs_cellheight = 30
    , gs_cellwidth = 100
    }

-- Show debug info, such as WorkspaceId (XZY).
l_gridShowDebugInfo :: X ()
l_gridShowDebugInfo = do
  windowSet <- gets windowset
  let
    xzy = l_XZYFromWindowSet windowSet
    wid = show xzy
    zGrp = show . l_ZCoordToGroup $ l_ZFrom xzy
    focusedWindow = case W.stack . W.workspace $ W.current windowSet of
      Just s -> show $ W.focus s
      Nothing -> "None"
    -- stringColorizer uses the _snd_ part of the pairs in debugInfo, not fst.
    debugInfo =
      [ ("WINDOWID: " ++ focusedWindow, "XID")
      , ("XZY: " ++ wid, "XZY")
      , ("ZGroup: " ++ zGrp, "ZGroup")
      ]
  _ <- gridselect (gsconfig2 stringColorizer) debugInfo
  return ()
  where
  gsconfig2 colorizer = (buildDefaultGSConfig colorizer)
    { gs_cellheight = 30
    , gs_cellwidth = 200
    }

-- If the current workspace has windows, move focus within the current worpace.
-- If the current workspace is empty, this analagous to H-{h,l}).
l_viewWindowOrWorkspace :: Direction1D -> X ()
l_viewWindowOrWorkspace dir = do
  windowSet <- gets windowset
  let
    currentX = l_XFromWid . W.tag . W.workspace $ W.current windowSet
    -- "wsAcrossX" has all visible stacks from other screens in the current Y
    -- cordinate (excluding the current stack in the X coordinate).
    wsAcrossX = (if dir == Next then id else reverse)
      . map snd
      . l_wrapWithout (\(x, _) -> x == currentX)
      . map (\ww -> (l_XFromWid $ W.tag ww, ww))
      . sortOn (l_XFromWid . W.tag)
      . map W.workspace
      $ W.screens windowSet
    nextVisible = head wsAcrossX
    currentStack = W.stack $ W.workspace $ W.current windowSet
    moveFocus = windows $ if dir == Next
        then W.focusDown
        else W.focusUp
    moveFocusWithHop = do
      focusTowardOppositeEdge nextVisible
      windows . W.view . W.tag $ nextVisible
      l_maybeShowHiddenNonEmptyZCount
    focusTowardOppositeEdge ww = case W.stack ww of
      Just _ -> resetFocus ww
      Nothing -> return ()
    resetFocus = windows . l_modifyVisible (if dir == Next then focusTop else focusBottom)
    focusTop s@(W.Stack t ls rs)
      | null ls && null rs = s
      | null ls = s
      | null rs = W.focusDown' s
      | otherwise = W.Stack (last ls) [] (reverse (init ls) ++ [t] ++ rs)
    focusBottom s@(W.Stack t ls rs)
      | null ls && null rs = s
      | null rs = s
      | null ls = W.focusUp' s
      | otherwise = W.Stack (last rs) (reverse (init rs) ++ [t] ++ ls) []
    focusedOnEdge = case currentStack of
      Just s -> null ((if dir == Next then W.down else W.up) s)
      Nothing -> True
    action
      | isNothing currentStack || focusedOnEdge = do
          l_promoteHidden
          moveFocusWithHop
          l_maybeShowHiddenNonEmptyZCount
      | otherwise = moveFocus
  action

l_viewAcrossX :: ScreenId -> X ()
l_viewAcrossX sc = do
  l_promoteHidden
  flip whenJust (windows . W.view) =<< screenWorkspace sc
  l_promoteHidden

l_promoteHidden :: X ()
l_promoteHidden = do
  whenX (l_currentWindowCountIs (==0)) $
    moveTo Prev $ l_searchZ (WQ NonEmpty [])
  l_maybeShowHiddenNonEmptyZCount

-- Like XMonad.StackSet.modify, but act on the Stack found in the given
-- workspace, if it is currently visible to the user.
l_modifyVisible :: Eq i =>
  (W.Stack a -> W.Stack a)
  -> W.Workspace i l s
  -> W.StackSet i l a sid sd
  -> W.StackSet i l a sid sd
l_modifyVisible editStack ww windowSet
  | wwIsCurrent = W.modify Nothing (Just . editStack) windowSet
  | otherwise = maybe windowSet replaceIfFound (find ((==W.tag ww) . W.tag . W.workspace) $ W.visible windowSet)
  where
  wwIsCurrent = (==W.tag ww) $ W.currentTag windowSet
  -- No need to bind the first thing here, because we reference windowSet from
  -- the outer scope. Closures!
  replaceIfFound _ = windowSet
    { W.visible = l_replaceIf (\s -> W.tag (W.workspace s) == W.tag ww) editStackOfScreen $ W.visible windowSet }
  editStackOfScreen s = s
    { W.workspace = (W.workspace s)
      { W.stack = fmap editStack . W.stack $ W.workspace s }}

l_replaceIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
l_replaceIf p f = map (\a -> if p a then f a else a)

-- Focus on the given Window `a', but instead of forcing the Workspace to show
-- up in the current Xine screen, _go_ to that Window. If the Window is already
-- located in the given WorkspaceId `wid', just view that Workspace. If it
-- doesn't reside in `wid', move it there first.
--
-- We assume that wid is in the current YCoord (otherwise, it's a NOP).
l_viewByGoing :: (WorkspaceId, Window) -> WindowSet -> WindowSet
l_viewByGoing (wid, a) windowSet
  | null (wws windowSet) || invalidWindow = windowSet
  | l_YFromWid wid /= l_YFromWid (W.currentTag windowSet) = windowSet
  | l_windowIsInWid wid a windowSet = f windowSet
  | otherwise = f $ W.shiftWin wid a windowSet
  where
  invalidWindow = notElem a $ W.allWindows windowSet
  f wx
    | isHidden wid wx = W.focusWindow a . l_promoteFromHidden wx . head $ wws wx
    | otherwise = W.focusWindow a wx
  isHidden b wx
    = elem b
    . map W.tag
    $ W.hidden wx
  wws wx =
    [ b
    | b <- W.workspaces wx
    , W.tag b == wid
    ]

l_windowIsInWid :: WorkspaceId -> Window -> WindowSet -> Bool
l_windowIsInWid wid a windowSet = (== Just wid) $ lookup a
  [ (a', W.tag ww)
  | ww <- W.workspaces windowSet
  , a' <- W.integrate' $ W.stack ww
  ]

-- Try to find a workspace based on the given WorkspaceQuery, but inside the
-- current XCoord and YCoord. In other words, flip through the ZCoords available
-- on the current physical screen (X and Y coordinates stay constant).
l_searchZ :: WorkspaceQuery -> WSType
l_searchZ q = WSIs $ do
  windowSet <- gets windowset
  let
    (x, _, y) = l_CoordsFromWid . W.tag . W.workspace $ W.current windowSet
    predicate ww = l_resolveQuery q ww
      && l_XFromWid (W.tag ww) == x
      && l_YFromWid (W.tag ww) == y
  return predicate

-- Like l_searchZ, but instead of only searching for the one workspace that
-- satisfies the WorkspaceQuery, return an empty workspace if the search fails.
l_searchZPreferNonEmpty :: X WSType
l_searchZPreferNonEmpty = do
  xzyNext <- findWorkspace getSortByIndex Next qNonEmpty 1
  xzyCurrent <- gets (W.tag . W.workspace . W.current . windowset)
  return $ if xzyNext == xzyCurrent
    then qEmpty
    else qNonEmpty
  where
  qEmpty = l_searchZ (WQ Empty [])
  qNonEmpty = l_searchZ (WQ NonEmpty [])

-- Like l_searchZ, but always return a XZY in a given ZGroup. When choosing
-- among XZYs in the ZGroup, choose the one with the fewest number of windows.
l_searchZPreferZGroup :: ZGroup -> X WorkspaceId
l_searchZPreferZGroup zGroup = do
  windowSet <- gets windowset
  let
    numScreens = length $ W.screens windowSet
    xzyCurrent = l_XZYFromWid . W.tag $ W.workspace $ W.current windowSet
    (XZY (x, _, y)) = l_XZYFromWindowSet windowSet
    -- Get all XZYs at the current XCoord/YCoord combination that belong to
    -- zGroup.
    xzys = filter ((==x) . l_XFrom) $ l_XZYsFrom numScreens zGroup id y
    xzysOfGroup =
      [ (l_XZYFromWid wid, length $ W.integrate' stack)
      | (W.Workspace wid _ stack) <- W.workspaces windowSet
      , elem wid $ map show xzys
      ]
    xzyPicked
      | null xzysOfGroup = xzyCurrent
      | otherwise = fst . head $ l_multiSort xzysOfGroup
        [ comparing snd
        , comparing fst
        ]
  return $ show xzyPicked

l_multiSort :: [a] -> [a -> a -> Ordering] -> [a]
l_multiSort sortMe dimensions = sortBy (l_compareDimensions dimensions) sortMe

-- The reason why the type signature is not "[a -> a -> Ordering]" for the
-- predicates is that we are comparing a single list of things, but are using
-- arbitrary predicates "ps" that can change to type of "a" to be anything, as
-- long as that resulting type is an Orderable thing. So, if we are sorting
-- "[a]" in l_multiSort and are using different orderable attributes of "a",
-- there must come point in time when the different attributes of "a" get
-- compared by their underlying Ordering conversion.
--
-- Put another way, if we do have [a -> a -> Ordering] here for ps, then
-- obviously we are only sorting in 1 dimension (the "a" dimension), which is
-- not what we want.
l_compareDimensions :: [a -> b -> Ordering] -> a -> b -> Ordering
l_compareDimensions ps x y
  = headDef EQ
  . dropWhile (==EQ)
  $ map (\p -> p x y) ps

-- Resolve the given WorkspaceQuery against the Workspace/WindowSpace. This is
-- also where we resolve the ZGroup membership exclusion list.
l_resolveQuery :: WorkspaceQuery -> WindowSpace -> Bool
l_resolveQuery (WQ hasWindows zGroupMemberships) ww
  = l_queryEmptiness hasWindows ww
  && l_queryZGroupMemberships zGroupMemberships ww

l_queryEmptiness :: WindowPopulation -> WindowSpace -> Bool
l_queryEmptiness hasWindows ww = case hasWindows of
  Empty -> isNothing $ W.stack ww
  NonEmpty -> isJust $ W.stack ww

l_queryZGroupMemberships :: ZGroupMemberships -> WindowSpace -> Bool
l_queryZGroupMemberships zGroupMemberships ww = all
  (\(zGroup, expectedResult) -> expectedResult == l_inGroup zGroup ww)
  zGroupMemberships

l_inGroup :: ZGroup -> WindowSpace -> Bool
l_inGroup zGroup ww = (l_ZCoordToGroup . l_ZFromWid $ W.tag ww) == zGroup

-- Move the focused window to the given Workspace, and then view that Workspace.
l_shiftAndView :: WorkspaceId -> WindowSet -> WindowSet
l_shiftAndView wid = W.view wid . W.shift wid

-- Like XMonad.ManageHook.doShift, but follow the moved window to its new home.
-- We use l_viewY just in case the destination WorkspaceId is not in the current
-- YCoord.
l_shiftAndViewAsHook :: WorkspaceId -> ManageHook
l_shiftAndViewAsHook wid = do
  a <- ask
  composeAll
    [ doF =<< liftX (l_viewY (l_YFromWid wid) False >> return id)
    , doF $ l_viewByGoing (wid, a)
    ]

l_currentWindowCountIs :: (Int -> Bool) -> X Bool
l_currentWindowCountIs predicate = do
  windowSet <- gets windowset
  let
    windowCount = length . W.integrate' . W.stack . W.workspace $ W.current windowSet
  pure $ predicate windowCount

l_windowCountIs :: (Int -> Bool) -> WorkspaceId -> X Bool
l_windowCountIs predicate wid = do
  windowSet <- gets windowset
  let
    xzy = l_XZYFromWid wid
    windowCount = head
      [ length . W.integrate' $ W.stack ww
      | ww <- W.workspaces windowSet
      , W.tag ww == show xzy
      ]
  pure $ predicate windowCount

l_workspaceIsEmpty :: XZY -> X Bool
l_workspaceIsEmpty xzy = do
  windowSet <- gets windowset
  return . isJust $ listToMaybe
    [ ww
    | ww <- W.workspaces windowSet
    , W.tag ww == show xzy
    , isNothing $ W.stack ww
    ]

-- Terminals.
l_term :: String -> String
l_term hostname = case hostname of
  "k1" -> "LIBGL_ALWAYS_SOFTWARE=1 " <> invocation
  _ -> invocation
  where
  invocation = "wezterm"

l_isPortraitMonitorLayout :: String -> Bool
l_isPortraitMonitorLayout givenHost = any (`isPrefixOf` givenHost) portraitHosts
  where
  portraitHosts = ["k0"]

l_keyBindings :: String
  -> Int
  -> XConfig Layout
  -> M.Map (KeyMask, KeySym) (X ())
l_keyBindings hostname numScreens conf@XConfig {XMonad.modMask = hypr}
  = M.fromList
  . map (second (\f -> f >> l_resetMouse True)) $
  -- Close focused window.
  [ ((hyprS,  xK_q            ), kill)

  -- Rotate through the available layout styles.
  , ((hypr,   xK_t            ), sendMessage NextLayout)

  -- Reset the layouts on the current workspace to default.
  , ((hypr,   xK_equal        ), setLayout $ XMonad.layoutHook conf)

  -- Move focus to the next/prev window, on the current set of current + visible
  -- screens.
  , ((hypr,   xK_j            ), l_viewWindowOrWorkspace Next)
  , ((hypr,   xK_k            ), l_viewWindowOrWorkspace Prev)

  -- Swap the focused window with the next/prev window.
  , ((hyprS,  xK_j            ), windows W.swapDown)
  , ((hyprS,  xK_k            ), windows W.swapUp)

  -- Unfloat/float window.
  , ((hypr,   xK_f            ), withFocused $ windows . W.sink)
  , ((hyprS,  xK_f            ), withFocused $ windows . flip W.float relativeDimenions)

  -- Shrink/Expand work on the Master window; the Mirror* counterparts do the
  -- same (although, from the looks of it, the definitions are somehow
  -- *reversed*), but for a slave window.
  , ((hyprS,  xK_bracketleft  ), shrinkExpand Shrink MirrorExpand)
  , ((hyprS,  xK_bracketright ), shrinkExpand Expand MirrorShrink)

  -- Increment/decrement the number of windows in the master area.
  , ((hypr,   xK_m            ), sendMessage (IncMasterN 1))
  , ((hyprS,  xK_m            ), sendMessage (IncMasterN (-1)))

  , ((hypr,   xK_g            ), l_gridSelectWithinY)
  , ((hypr,   xK_s            ), l_gridShowDebugInfo)

  -- Lock screen.
  , ((hypr,   xK_Escape       ), spawnSelected def
    [ "~/syscfg/script/susp.sh"
    , "xscreensaver-command -lock"])

  -- Toggle window borders.
  , ((hypr,   xK_b            ), withFocused toggleBorder)

  -- Reapply l_manageHook against the focused window.
  , ((hypr,   xK_Return       ), l_resetWindow numScreens)

  -- Go to empty Workspace, on the current YCoord, in the current physical
  -- screen. For shifting an existing window to an empty Workspace, only do so
  -- if there is indeed a window to work with in the current Workspace (i.e., if
  -- all ZCoords at this X/Y-coordinate pair is full, do nothing).
  , ((hypr,   xK_o            ), do
      moveTo Next $ l_searchZ (WQ Empty [])
      l_maybeShowHiddenNonEmptyZCount)
  , ((hyprS,  xK_o            ), whenX (l_currentWindowCountIs (>0)) $ do
      doTo Next (l_searchZ (WQ Empty [])) getSortByIndex (windows . l_shiftAndView)
      l_maybeShowHiddenNonEmptyZCount)
  ]
  ++
  -- Go to any non-empty Workspace only within the Z-Axis (X and Y remain
  -- untouched) The Z-axis direction is either Next or Prev (depending on the
  -- key). If we use the Shift key, move the current window to that direction,
  -- unless there is only 1 window.
  [ ((modifier, key), action >> l_maybeShowHiddenNonEmptyZCount)
  | (key, dir) <-
    [ (xK_n, Next)
    , (xK_p, Prev)
    ]
  , (modifier, action) <-
    [ (hypr, moveTo dir $ l_searchZ (WQ NonEmpty []))
    , (hyprS, whenX
        (l_currentWindowCountIs (>0))
        ((\wst -> doTo dir wst getSortByIndex (windows . l_shiftAndView))
          =<< l_searchZPreferNonEmpty))
    ]
  ]
  ++
  -- H-{h,l}: Switch focus across X-axis (prev/next physical screen).
  [((hypr, key), l_viewAcrossX =<< sc)
  | (key, sc) <- [(xK_h, screenBy (-1)), (xK_l, screenBy 1)]
  ]
  ++
  -- H-S-{h,l}: Move focused window along X-axis. The difference here versus the
  -- vanilla XMonad behavior is that we not only move the focused window, but
  -- _move focus to it_ afterwards. This way, repeatedly pressing this binding
  -- merely moves the focused window along; otherwise we just end up moving all
  -- windows out of the current screen, which is not as useful in practice.
  --
  -- It is worth noting that this binding does nothing if there is no window in
  -- the current workspace to move.
  [((hyprS, key), whenX
    (l_currentWindowCountIs (>0))
    (do
      windowSet <- gets windowset
      let
        widCur = W.tag . W.workspace $ W.current windowSet
      wst <- l_searchZPreferNonEmpty

      -- Move focused window to next workspace.
      widNext <- fromJust <$> (screenWorkspace =<< screenBy dir)
      windows $ W.shift widNext

      -- On the old workspace, try to promote hidden nonempty workspace if we're
      -- staring at an empty one.
      whenX (l_windowCountIs (==0) widCur) $
        doTo Prev wst getSortByIndex (windows . l_shiftAndView)
      l_maybeShowHiddenNonEmptyZCount

      -- Shift focus to workspace.
      windows $ W.view widNext

      -- Show hidden workspace count, if any.
      l_maybeShowHiddenNonEmptyZCount))
      | (key, dir) <- [(xK_h, -1), (xK_l, 1)]
  ]
  ++
  -- Show the next set of workspaces by moving in the Y direction. If at the
  -- edge, do NOT cycle; instead, popup a message saying that we're at the edge
  -- of the Y coordinate.
  [ ((hyprA,  xK_j            ), l_viewYNonEmpty Next)
  , ((hyprAS, xK_j            ), whenX
                                  (l_currentWindowCountIs (>0))
                                  (yEdgeGuard Next $ l_shiftYDir Next))
  , ((hyprA,  xK_k            ), l_viewYNonEmpty Prev)
  , ((hyprAS, xK_k            ), whenX
                                  (l_currentWindowCountIs (>0))
                                  (yEdgeGuard Prev $ l_shiftYDir Prev))
  , ((hyprA,  xK_Return       ), yEdgeGuard Next $ l_viewYDir Next False)
  , ((hyprA,  xK_BackSpace    ), yEdgeGuard Prev $ l_viewYDir Prev False)
  , ((hypr,   xK_y            ), l_viewNthY 1 False)
  , ((hypr,   xK_w            ), l_viewNthY 2 False)
  ]
  ++
  -- hypr-[0..9]: Switch to YCoord N.
  -- hyprS-[0..9]: Move focused window to YCoord N, preserving its XCoord.
  -- NOTE: Depending on the machine, we change the order of keys to optimize
  -- for the keyboard layout used on the machine. The order in
  -- `forQwertyKeyboard' is coincidentally optimized for that layout, because
  -- the "1", "2", "3" keys etc. are nearest the left side of the keyboard
  -- where we have our XMonad mod key (CapsLock remapped to Hyper key). In
  -- `forZQKeyboard', the numpad home row gets priority.
  [((hypr .|. mask, k         ), f y)
    | (y, k) <- zip l_YCoords' $ if l_isPortraitMonitorLayout hostname
      then forZQKeyboard
      else forQwertyKeyboard
    , (f, mask) <-
      [ (flip l_viewY False, 0)
      , (whenX (l_currentWindowCountIs (>0)) . l_shiftY, shiftMask)
      ]
  ]
  ++
  -- Launch apps.
  [ ((hypr,   xK_i            ), spawnSelected def ["qutebrowser", "firefox", "chromium"])
  , ((hypr,   xK_e            ), spawnSelected def [l_term hostname, l_term hostname, "xterm"])
  -- Backup binding to launch a terminal in case our Hyper key (hypr) is
  -- unavailable. This happens whenever we unplug/replug our keyboard, and a
  -- terminal isn't already showing in a window somewhere to be able to call
  -- ~/syscfg/xmonad/xenv.sh to re-initialize the Hyper key. Because the
  -- Hyper key is used exclusively to maneuver around Xmonad, we need a
  -- non-Hyper-key binding to launch a terminal to bootstrap ourselves back in
  -- with initkeys.sh.
  , ((altS,   xK_e            ), spawnSelected def [l_term hostname, l_term hostname, "xterm"])
  , ((hypr,   xK_u            ), spawnSelected def ["emacsclient", "emacs"])
  ]
  where
  yEdgeGuard dir = l_if (l_atYEdge dir) (return ())
  shrinkExpand master slave = if l_isPortraitMonitorLayout hostname
    then sendMessage slave
    else sendMessage master
  hyprS = hypr .|. shiftMask
  -- Alias "altMask" for left alt key.
  altMask = mod1Mask
  altS = altMask .|. shiftMask
  hyprA = hypr .|. altMask
  hyprAS = hypr .|. altMask .|. shiftMask
  relativeDimenions
    = W.RationalRect marginLeft marginTop windowWidth windowHeight
    where
    marginLeft = 1/6
    marginTop = 1/6
    windowWidth = 2/3
    windowHeight = 2/3
  forZQKeyboard =
    [ xK_0
    , xK_4
    , xK_5
    , xK_6
    , xK_7
    , xK_8
    , xK_9
    , xK_1
    , xK_2
    , xK_3
    ]
  forQwertyKeyboard =
    [ xK_1
    , xK_2
    , xK_3
    , xK_4
    , xK_5
    , xK_6
    , xK_7
    , xK_8
    , xK_9
    , xK_0
    ]
  l_YCoords' = map Y (if l_isPortraitMonitorLayout hostname
    -- For Zq, we just use the plain ordering. In practice this doesn't even
    -- matter that much because we just use the H-M-j/k bindings for navigating
    -- across Y coordinates.
    then [0,1,2,3,4,5,6,7,8,9]
    -- For Qwerty, we want 0 to be last, because it's printed last in the number
    -- row.
    else [1,2,3,4,5,6,7,8,9,0])

l_mouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
l_mouseBindings _ = M.fromList
  -- hypr-button1 (left-click): Set the window to floating mode and move by
  -- dragging.
  [ ( (altGr, button1)
  , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
  )
  -- hypr-button2 (middle-click): Raise the window to the top of the stack.
  -- This binding is almost never used, but we keep it here for instructive
  -- purposes.
  , ((altGr, button2), \w -> focus w >> windows W.shiftMaster)
  ,
  -- hypr-button3 (right-click): Set the window to floating mode and resize by
  -- dragging.
  ( (altGr, button3)
  , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
  )
  -- You may also bind events to the mouse scroll wheel (button4 and button5).
  ]
  where
  altGr = mod5Mask

-- Tall is the default tiling algorithm, which partitions the screen into two
-- panes. It takes three arguments x y z, where x is the default number of
-- windows in the master pane, y is the percent of the screen to increment by
-- when resizing panes, and z is the default proportion of the screen occupied
-- by the master pane.
l_layoutHook :: Choose (Mirror Tall) (Choose Tall (XLL.ModifiedLayout WithBorder Full)) Window
l_layoutHook = Mirror (tiled 1) ||| tiled 1 ||| noBorders Full
  where
  tiled nmaster = Tall nmaster delta ratio
  delta = 3/100
  ratio = 1/2

l_layoutNoMirror :: Choose ResizableTall (XLL.ModifiedLayout WithBorder Full) Window
l_layoutNoMirror = ResizableTall 0 (3/100) (1/2) [] ||| noBorders Full

-- ManageHook: Execute arbitrary actions and WindowSet manipulations when
-- managing a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular workspace.
--
-- To find the property name associated with a program, use
--
--  xprop | grep WM_CLASS
--
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that 'className'
-- and 'resource' are used below. E.g., if
--
--   WM_CLASS(STRING) = "Qt-subapplication", "VirtualBox"
--
-- then Qt-subapplication is resource, and VirtualBox is className.
l_manageHook :: Int -> ManageHook
l_manageHook numScreens = composeOne $
  [ className =? "Gimp"               -?> doFloat
  , className =? "Agave"              -?> doCenterFloat
  , resource  =? "desktop_window"     -?> doIgnore
  , resource  =? "kdesktop"           -?> doIgnore
  , resource  =? "floatme"            -?> doCenterFloat
  -- Move browsers to the ZGNet ZGroup. If all ZCoords in ZGNet are full,
  , className =? "qutebrowser"        -?> l_shiftAndViewAsHook =<< toZGNet
  , className =? "Chromium-browser"   -?> l_shiftAndViewAsHook =<< toZGNet
  , resource  =? "Navigator"          -?> l_shiftAndViewAsHook =<< toZGNet
  , className =? "Blender:Render"     -?> doFloat
  , resource  =? "Browser"            -?> doFloat
  , className =? "Xsane"              -?> doFloat
  , className =? "Glade"              -?> doFloat
  , className =? "Anki"               -?> doFloat
  ]
  ++
  -- This is useful for auto-moving a terminal screen we spawn elsewhere in
  -- this config file to a particular workspace.
  map
    (\xzy ->
      resource =? ("atWorkspace_" ++ show xzy) -?> l_shiftAndViewAsHook (show xzy))
    (l_XZYs numScreens)
  ++
  -- Force new windows down (i.e., if a screen has 1 window (master) and we
  -- spawn a new window, don't become the new master window). See "Make new
  -- windows appear 'below' rather than 'above' the current window" at
  -- https://wiki.haskell.org/Xmonad/Frequently_asked_questions.
  [ return True -?> doF W.swapDown
  ]
  where
  toZGNet = liftX $ l_searchZPreferZGroup ZGNet

l_startupHook :: String -> X ()
l_startupHook hostname = do
  -- When we start up, we visit the default (starting) Y Coord. Record it.
  l_updateYHist
  -- Kill ~/.xmonad/xmonad.state (this file makes sense when you have 5 or 10
  -- workspaces, but its utility breaks down quickly when you have as many
  -- workspaces as we do).
  spawn "rm -f ~/syscfg/xmonad/xmonad.state"
  -- Setup keyboard.
  spawn "~/syscfg/xmonad/xenv.sh"
  windowSet <- gets windowset
  y <- gets (l_YFromWindowSet . windowset)
  let
    numScreens = length $ W.screens windowSet
  -- Spawn one terminal in every screen at the "Work" ZGroup at the current
  -- YCoord (but only if that screen is empty). We have to feed in `(take 1)' in
  -- order to spawn terminals in a single ZCoord.
  --
  -- Each of these terminals also spawn a tmux client instance.
  mapM_
    (\xzy -> whenX (l_workspaceIsEmpty xzy)
      (spawn $ l_term hostname ++ " --class atWorkspace_" ++ show xzy))
    $ l_XZYsFrom numScreens ZGWork (take 1) y
  -- The emacs daemon also starts up org-roam-server-mode, which turns on an
  -- HTTP server to serve org-roam files over port 8010.
  spawn "emacs --daemon"
  when (hostname == "k0") $ do
    spawn "qutebrowser"
  spawn "~/syscfg/script/startup_hook.sh"

-- Reset the location of the mouse pointer, with the destination depending on
-- the type of window that is currently focused.
l_resetMouse :: Bool -> X ()
l_resetMouse alwaysReset = do
  windowSet <- gets windowset
  dpy <- asks display
  root <- asks theRoot
  (_,_,_,_,_,_,_,inputMask) <- io $ queryPointer dpy root
  let
    currentStack = W.stack . W.workspace $ W.current windowSet
    moveMouse stack = banish
      . snd
      =<< foldM step (W.focus stack, LowerRight) windowPropToDest
    maybeMoveMouse stack = do
      mouseIsMoving <- asks mouseFocused
      drag <- gets dragging
      let
        mouseBeingUsed
          = mouseIsMoving
          || isJust drag
          -- Detect if we are pressing down the 1st mouse button (traditionally
          -- called "Left Click" for right-handed mice). This catches the use
          -- case where a Chromium tab is being clicked, then dragged onto
          -- another Chromium window's tab bar area. In this case, we don't want
          -- to reset the mouse position. Granted, the window will still jump
          -- around, because XMonad will try to tile it and untile it as it
          -- creates/destroys windows, but this is the vanilla XMonad behavior
          -- anyway without l_resetMouse. Without this boolean check, we end up
          -- repeatedly resetting the mouse coordinates (in a way that
          -- interferes with Chromium's vision of where the pointer is), making
          -- it basically impossible to move a tab back onto another tab bar
          -- area (a workaround is to first float the window and then drag the
          -- tag, but this is too much work).
          --
          -- We explicitly choose button1Mask, because that's the only button we
          -- use to drag Chromium tabs around.
          --
          -- NOTE: Firefox does not have this issue of windows jumping around
          -- when we move tabs across windows.
          || inputMask .&. button1Mask /= 0
      when (not mouseBeingUsed) $ moveMouse stack
    f stack
      | alwaysReset = moveMouse stack
      | otherwise = do
        ev <- asks currentEvent
        whenJust ev $ \case
          -- MapRequestEvent is triggered when new windows are created.
          -- Conversely, UnmapEvent is triggered when windows are deleted.
          MapRequestEvent {} -> maybeMoveMouse stack
          UnmapEvent {} -> maybeMoveMouse stack
          _ -> pure ()
  whenJust currentStack f
  where
  step s@(w, _) (p, destNew) = do
    b <- hasProperty p w
    if b
      then return (w, destNew)
      else return s
  windowPropToDest =
    [ (ClassName "org.wezfurlong.wezterm", LowerLeft)
    , (ClassName "Emacs", LowerLeft)
    , (ClassName "qutebrowser", UpperRight)
    , (ClassName "Chromium-browser", UpperRight)
    , (ClassName "Firefox", UpperRight)
    , (ClassName "firefox", UpperRight)
    ]

-- Reset the focused window by running l_manageHook against it. This way, we can
-- move a window back to its correct ZCoord. The fancy appEndo/<=< stuff is from
-- `XMonad.Actions.WindowGo.ifWindow'.
l_resetWindow :: Int -> X ()
l_resetWindow numScreens = do
  windowSet <- gets windowset
  let
    currentStack = W.stack . W.workspace $ W.current windowSet
  whenJust
    currentStack
    (windows . appEndo <=< runQuery (l_manageHook numScreens) . W.focus)

main :: IO ()
main = do
  numScreens <- countScreens
  hostname <- fmap nodeName getSystemID
  if l_isPortraitMonitorLayout hostname
    then xmonad (myconf hostname numScreens) {layoutHook = l_layoutNoMirror}
    else xmonad $ myconf hostname numScreens
  where
  myconf hostname numScreens = def
    { terminal           = "wezterm"
    , focusFollowsMouse  = True
    , clickJustFocuses   = True
    , borderWidth        = 1
    -- Use 'mod3' from 'xmodmap' output as our 'modMask' key. We alias it
    -- (XMonad.modMask) it as `hypr` in our configuration above. Either before
    -- or immediately after XMonad starts, 'mod3' should be populated as the
    -- Hyper key (as this is how we call it above). This is handled usually by
    -- xsession scripts. On NixOS, it is baked in directly to the system
    -- configuration file under the
    -- `services.xserver.displayManager.sessionCommands` option.
    , modMask            = mod3Mask
    , workspaces         = map show $ l_XZYs numScreens
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#ffffff"
    , keys               = l_keyBindings hostname numScreens
    , mouseBindings      = l_mouseBindings
    , layoutHook         = l_layoutHook
    , manageHook         = l_manageHook numScreens
    , handleEventHook    = l_eventHook
    , logHook            = l_resetMouse False
    , startupHook        = l_startupHook hostname
    }
