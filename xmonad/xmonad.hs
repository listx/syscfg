module Main where

import Control.Monad
  ( when )
import Data.List
  ( find
  , foldl'
  , isPrefixOf
  , nub
  , sortBy
  )
import Data.Maybe
  ( isJust
  , isNothing
  , listToMaybe
  )
import Data.Ord
  ( comparing
  )
import Safe
  ( headDef )
import System.Exit
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
  , shiftTo
  )
import XMonad.Actions.GridSelect
  ( def
  , gridselect
  , runSelectedAction
  , spawnSelected
  , stringColorizer
  , buildDefaultGSConfig
  , gs_cellheight
  , gs_cellwidth
  )
import XMonad.Actions.NoBorders
  ( toggleBorder )
import XMonad.Actions.Warp
  ( warpToWindow )
import XMonad.Hooks.ManageHelpers
  ( (-?>)
  , doCenterFloat
  , composeOne
  )
import XMonad.Layout.IndependentScreens
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
import XMonad.Util.NamedWindows
  ( getName )
import XMonad.Util.WorkspaceCompare
  ( getSortByIndex )
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

type XCoord = String
type YCoord = String
type ZCoord = String

-- Remind ourselves of our "<X>_<Z>:<Y>" naming scheme of WorkspaceIds. The
-- "<Z>:<Y>" suffix of each WorkspaceId is called "VirtualWorkspace" by
-- IndependentScreens. See discussion below.
type ZY = VirtualWorkspace
-- Each WorkspaceId, aka Workspace name, is the unique cross-section of the X,
-- Z, and Y axes.
type XZY = WorkspaceId

-- Each Workspace has a name associated; internally by XMonad it is called a
-- "tag" and it is just a string (type synonym of "WorkspaceId"). We use a
-- combination of X, Y, and Z-axis coordinates to get a 3-dimensional view of
-- Workspaces. We do this by breaking up WorkspaceId into the following format:
-- "<XCoord>_<ZCoord>:<YCoord>". Thankfully, we get "<XCoord>_<ZCoord>" for free
-- by just using XMonad.Layout.IndependentScreens; that extension's
-- "withScreens" adds the "<XCoord>_" prefix for us for all <ZCoord> names we
-- define. (For reference, IndependentScreens calls the "<ZCoord>" part a
-- VirtualWorkspace, and the full "<XCoord>_<ZCoord>" part a PhysicalWorkspace.)
--
-- So far in our discussion we are using just the X and Z coordinates. The
-- H-h/H-l bindings move the active workspace (or window) focus across Xinerama
-- screens in the X-axis direction (the vanilla XMonad bindings are
-- Modkey-{Q,W,E}). We chose the name X-axis for this direction because on most
-- multihead Xinerama setups, the monitors are placed horizontally in a
-- left-to-right fashion. To move into our Z axis, we use H-n; this binding
-- switches the workspace on the current Xinerama screen only and leaves all
-- other screens as-is. Intuitively, we can imagine this 2-dimensional scheme as
-- if each XMonad worspace is a playing card in a deck of cards. Each Xinerama
-- screen has its own unique deck. Not to be pedantic, but for those unfamiliar
-- with XMonad, each "playing card" here is its own "Desktop" with multiple GUI
-- windows of applications.
--
-- Now we add in our final layer, the Y dimension, by appending ":<YCoord>" to
-- the PhysicalWorkspace names. The H-M-j/H-M-k bindings move up and down the Y
-- axis. Continuing with our playing card analogy, it's as if each <YCoord> in
-- the Y axis has its own independent array of decks. The most interesting thing
-- here is that moving up and down the Y axis changes *all* screens (every deck
-- must change!). The effect is quite dramatic (and refreshing on my 4-monitor
-- setup) and really shows off the power of Xinerama. This simulates the
-- workflow demonstrated in https://youtu.be/w5_36BBGoU0?t=3m30s, where multiple
-- screens all change content at once. The cool part about our setup is that we
-- have the added Z dimension (for each unique X and Y coordinate pair), giving
-- us much more room to place windows around. An important thing to keep in mind
-- here is that even though we change by the Y coordinate with H-M-j/H-M-k, we
-- actually change the Z coordinate as well if necessary on a per-screen basis
-- (for each Xinerama screen (XCoord)). This is because whenever we move up/down
-- the Y axis, we move to the *last used set of XZY workspaces at that YCoord*;
-- using our playing card example, each Y coordinate (array of decks) remembers
-- what ZCoord (playing card) each X screen (deck) was showing before focus
-- changed to a different Y coordinate. This "last seen" concept is made
-- possible with the "Seen" data type.
--
-- That's about it. Maybe one day this will become an XMonad extension...

-- We have 22 ZCoords. The number 22 is important because we have 22 keybindings
-- (H-[0-9] and H-[F1-F12]) that can in total work with 22 locations.
l_ZCoords :: [ZCoord]
l_ZCoords = take 22 $ map (:[]) ['a'..]

-- The number of connected Xinerama screens can vary across OS boots (e.g., when
-- we disconnect or connect an external monitor to a laptop). So we rely on
-- XMonad.Layout.IndependentScreens to give us the correct number of X
-- coordinates (see countScreens), and use it here as xineramaCount. (NOTE: we
-- do use countScreens multiple times, and it is unclear what will happen if we
-- change the number of Xinerama screens within a single X Session.)
l_XCoords :: Int -> [XCoord]
l_XCoords xineramaCount = map show $ take xineramaCount [(0::Int)..]

-- We have 10 Y coordinates; we can add more as necessary in the future.
l_YCoords :: [YCoord]
l_YCoords = map show $ take 10 [(0::Int)..]

-- Again, for reference our format for WorkspaceIds are "<x>_<z>:<y>".
l_XFrom :: XZY -> XCoord
l_XFrom = fst . break (=='_')
l_ZFrom :: XZY -> XCoord
l_ZFrom = drop 1 . fst . break (==':') . snd . break (=='_')
l_YFrom :: XZY -> YCoord
l_YFrom = drop 1 . dropWhile (/=':')

-- Group ZCoords by type. Usinge our playing card analogy, these groups are like
-- the "suits" of cards.
data ZGroup
  = ZGWork
  | ZGNet
  | ZGMisc
  | ZGSys
  deriving (Eq, Ord, Enum, Show)

l_ZYGroups :: [(ZY, ZGroup)]
l_ZYGroups =
  [ (z ++ ":" ++ y, zGroup)
  | (z, zGroup) <- l_ZCoordGroups
  , y <- l_YCoords
  ]

l_ZCoordToGroup :: ZCoord -> ZGroup
l_ZCoordToGroup z
  | elem z $ map (:[]) ['a'..'f'] = ZGWork
  | elem z $ map (:[]) ['g'..'i'] = ZGNet
  | elem z $ map (:[]) ['j'..'j'] = ZGSys
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

-- Generate fully qualified WorkspaceIds from u
l_XZYs :: Int -> [XZY]
l_XZYs xineramaCount =
  [ x ++ "_" ++ z ++ ":" ++ y
  | x <- l_XCoords xineramaCount
  , z <- l_ZCoords
  , y <- l_YCoords
  ]

-- Each time we change the Y coordinate, we record the ZCoord on each XCoord.
-- For simplicity, instead of storing the ZCoord and XCoord pair, we instead
-- store the entire XZY triplet (which is also the full, unique WorkspaceId). We
-- use XMonad.Util.ExtensibleState to store this state. See
-- https://stackoverflow.com/questions/40270793/user-state-in-xmonad
data Seen = Seen (H.Map YCoord [XZY]) (Maybe YCoord)
instance ExtensionClass Seen where
  initialValue = Seen H.empty Nothing

l_YFromWindowSet :: WindowSet -> YCoord
l_YFromWindowSet = l_YFrom . W.tag . W.workspace . W.current

l_CoordsFromWindowSet :: WindowSet -> (XCoord, ZCoord, YCoord)
l_CoordsFromWindowSet windowSet =
  ( l_XFrom xzy
  , l_ZFrom xzy
  , l_YFrom xzy
  )
  where
  xzy = W.tag . W.workspace $ W.current windowSet

l_YIncrementedBy :: Direction1D -> YCoord -> YCoord
l_YIncrementedBy dir y = show $ mod (op (read y) 1) (length l_YCoords)
  where
  op = if dir == Next then (+) else (-)

-- Given the X, Y, and ZGroup constraints, generate the full XZY coordinate
-- WorkspaceId by deciding on the ZCoord.
l_XZYFrom :: Int -> Int -> ZGroup -> YCoord -> XZY
l_XZYFrom xCoord xineramaCount zGroup y = x ++ "_" ++ z ++ ":" ++ y
  where
  -- Wrap xCoord if it is out of bounds.
  x = l_XCoords xineramaCount !! mod xCoord xineramaCount
  -- We just grab the very first ZCoord in the ZGroup.
  z = head $ l_ZGroupToZCoords zGroup

-- For a given ZGroup and YCoord, get a list of XZYs belonging to that ZGroup
-- for every XCoord. The pool of available ZCoords is modified once by
-- transformZCoords.
l_XZYsFrom :: Int -> ZGroup -> ([ZCoord] -> [ZCoord]) -> YCoord -> [XZY]
l_XZYsFrom xineramaCount zGroup transformZCoords y =
  [ x ++ "_" ++ z ++ ":" ++ y
  | x <- l_XCoords xineramaCount
  , z <- transformZCoords $ l_ZGroupToZCoords zGroup
  ]

-- Abstraction to help us search for a Workspace. See l_searchZ for more info.
data WorkspaceQuery = WQ HasWindows ZGroupMemberships
data HasWindows
  = Empty
  | NonEmpty
  deriving (Eq)

-- The boolean in the tuple determines whether we should check if the XZY in the
-- Workspace belongs to the ZGroup (True for yes, it belongs; False for no it
-- does not belong).
type ZGroupMemberships = [(ZGroup, Bool)]

-- This function was written to implemet the idea expressed in
-- https://www.reddit.com/r/xmonad/comments/7mawso/switch_workspaces_on_multiple_monitors_with_1/.
-- It involves less work for the user than XMonad.Actions.DynamicWorkspaceGroups
-- because we don't have to manually name workspace groups (basically for us,
-- every YCoord in l_YCoords is a "WorkspaceGroup").
l_viewY :: YCoord -> X ()
l_viewY yNext = do
  windowSet <- gets windowset
  let
    -- (1) Get current (soon to be previous) YCoord.
    yPrev = l_YFromWindowSet windowSet
    -- Make note of all XZYs at the current YCoord.
    xzys = map (\screen -> W.tag $ W.workspace screen) $ W.screens windowSet
  -- (2) Activate next YCoord's workspaces. After this operation, visually all
  -- screens will have switched their XZY coordinate to reflect yNext, not
  -- yPrev.
  l_activateY yNext
  -- (3) Save xzys of yPrev, so that if and when we switch back to it, we get
  -- back the same workspaces (and not just some random default set of XZYs).
  recordXZYs yPrev xzys

l_viewYDir :: Direction1D -> X ()
l_viewYDir dir = do
  windowSet <- gets windowset
  let
    yPrev = l_YFromWindowSet windowSet
    yNext = l_YIncrementedBy dir yPrev
  l_viewY yNext

-- Like l_viewYDir, but first search in the given direction if there are any
-- non-empty XZYs, and if so, view that YCoord. If all other YCoords are empty,
-- do nothing. Note that a YCoord could be non-empty, but that it currently is
-- viewing only empty workspaces; such a YCoord is still considered non-empty.
l_viewYNonEmpty :: Direction1D -> X ()
l_viewYNonEmpty dir = do
  (Seen hashmap _) <- XS.get :: X Seen
  windowSet <- gets windowset
  let
    yPrev = l_YFromWindowSet windowSet
    xzys = map (\screen -> W.tag $ W.workspace screen) $ W.screens windowSet
    searchDirection = if dir == Next then id else reverse
    -- We say "were" instead of "are", because we use the history stored in Seen
    -- (it is, technically, old information). It could be incorrect (such as
    -- when XMonad is restarted and Seen state is lost --- even though windows
    -- remain populated at various Workspaces).
    xzysWerePopulatedAtY y = (,) y $ case H.lookup y hashmap of
      Just xzys' -> not $ null
        [ xzy
        | ww <- W.workspaces windowSet
        , xzy <- xzys'
        , isJust $ W.stack ww
        , W.tag ww == xzy
        ]
      Nothing -> False
    yNexts
      = map fst
      . dropWhile ((==False) . snd)
      . map xzysWerePopulatedAtY
      . searchDirection
      $ l_wrapWithoutItem yPrev l_YCoords
  when (not $ null yNexts) $ do
    l_activateY $ head yNexts
    recordXZYs yPrev xzys

l_viewLastY :: X ()
l_viewLastY = do
  (Seen _ lastY) <- XS.get :: X Seen
  case lastY of
    Just y -> l_viewY y
    Nothing -> return ()

recordXZYs :: YCoord -> [XZY] -> X ()
recordXZYs y xzys = XS.modify
  (\(Seen hashmap _) -> Seen (H.insert y xzys hashmap) (Just y))

-- Make the list cyclic so that we start with the given item as the first item;
-- then remove this item.
--
-- Examples:
--
--  l_wrapWithoutItem 3 [0..5] => [4, 5, 0, 1, 2]
--  l_wrapWithoutItem 4 [0..5] => [5, 0, 1, 2, 3]
l_wrapWithoutItem :: (Eq a) => a -> [a] -> [a]
l_wrapWithoutItem x xs
  | null xs = xs
  | otherwise = drop 1 after ++ before
  where
  (before, after) = break (==x) xs

-- Make the given YCoord "active" by viewing its XZYs on all Xinerama screen(s).
-- If we viewed the YCoord before, present its XZYs that we recorded when we
-- switched away from it the last time around. Otherwise, show the default XZYs
-- for the YCoord.
l_activateY :: YCoord -> X ()
l_activateY y = do
  windowSet <- gets windowset
  (Seen hashmap _) <- XS.get :: X Seen
  let
    xineramaCount = length $ W.screens windowSet
    xzys = case H.lookup y hashmap of
      Just xzys' -> xzys'
      Nothing -> l_defaultXZYsForY y xineramaCount
  windows $ l_viewXZYs xzys

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
    , xzy' == xzy
    ]

-- As a reminder, XMonad.Core (confusingly) uses the type synonym WindowSpace to
-- mean "Workspace i l a". We call a WindowSpace "ww" to make the code a little
-- easier to read (better than using "workspace", considering how we already use
-- XMonad.StackSet.workspace as W.workspace).
l_promoteFromHidden :: WindowSet -> WindowSpace -> WindowSet
l_promoteFromHidden windowSet ww
  -- If the given XCoord matches the XCoord of the current (focused) screen, we
  -- have to promote ww to be W.current and demote the existing W.current screen
  -- to be hidden.
  | matchesXCoordFromWW . W.screen $ W.current windowSet = windowSet
    { W.current = (W.current windowSet) { W.workspace = ww }
    , W.hidden = (W.workspace $ W.current windowSet) : otherHidden
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
  matchesXCoordFromWW (S s) = show s == (l_XFrom $ W.tag ww)
  otherHidden = filter ((/= W.tag ww) . W.tag) $ W.hidden windowSet

-- Pick out a default set of workspaces at a particular level. We make sure to
-- pick 1 workspace for each physical screen (given by xineramaCount).
l_defaultXZYsForY :: YCoord -> Int -> [XZY]
l_defaultXZYsForY y xineramaCount
  = selectForEachXCoord (l_XCoords xineramaCount)
  . filter xzyIsOnYCoord
  $ l_XZYs xineramaCount
  where
  xzyIsOnYCoord xzy = (drop 1 . snd $ break (==':') xzy) == y
  selectForEachXCoord xCoords xzys =
    [ xzy
    | xzy <- xzys
    , elem (l_XFrom xzy) xCoords
    -- Select the xzy at the first ZCoord.
    , l_ZFrom xzy == head l_ZCoords
    ]

-- Move currently focused window over to the next YCoord. Then view that YCoord.
-- If there is no currently focused window, don't do anything. This is the
-- Y-axis analogue of the H-S-{h,l} bindings powered by l_shiftAndView.
l_shiftY :: Direction1D -> X ()
l_shiftY dir = do
  windowSet <- gets windowset
  (Seen hashmap _) <- XS.get :: X Seen
  let
    xineramaCount = length $ W.screens windowSet
    yPrev = l_YFromWindowSet windowSet
    yNext = l_YIncrementedBy dir yPrev
    x = l_XFrom . W.tag . W.workspace $ W.current windowSet
    -- Like in l_viewYDir, try to grab the XZY of the last used Workspace at the
    -- target xzy.
    xzys = case H.lookup yNext hashmap of
      -- Grab the xzy with the same XCoord as the current screen.
      Just xzys' -> xzys'
      Nothing -> l_defaultXZYsForY yNext xineramaCount
  flip
    whenJust
    (\xzy -> (windows $ W.shift xzy) >> l_viewYDir dir)
    (find ((==x) . l_XFrom) xzys)

-- Show all windows at the current YCoord; if selected, then view that window
-- and switch focus to it.
l_gridSelectWithinY :: X ()
l_gridSelectWithinY = do
  windowSet <- gets windowset
  let
    y = l_YFromWindowSet windowSet
    xineramaCount = length $ W.screens windowSet
    attachName (ww, a) = do
      b <- fmap show $ getName a
      return (b, (ww, a))
  windowsAtY <- sequence
    . map attachName
    $ concat
      [ map ((,) ww) $ W.integrate' $ W.stack ww
      | ww <- W.workspaces windowSet
      , xzy <- l_XZYs xineramaCount
      , W.tag ww == xzy
      , l_YFrom xzy == y
      ]
  selected <- gridselect (gsconfig2 colorizeByXCoord) windowsAtY
  whenJust selected
    (windows . l_viewGently)
  where
  colorizeByXCoord (ww, _) = stringColorizer . l_XFrom $ W.tag ww
  gsconfig2 colorizer = (buildDefaultGSConfig colorizer)
    { gs_cellheight = 30
    , gs_cellwidth = 100
    }

l_viewGently :: (WindowSpace, Window) -> WindowSet -> WindowSet
l_viewGently (ww, a) windowSet
  | isHidden a = W.focusWindow a $ l_promoteFromHidden windowSet ww
  | otherwise = W.focusWindow a windowSet
  where
  isHidden window
    = not
    . or
    . map (elem window . W.integrate' . W.stack . W.workspace)
    $ W.screens windowSet

-- Try to find a workspace based on the given WorkspaceQuery, but inside the
-- current XCoord and YCoord. In other words, flip through the ZCoords available
-- on the current Xinerama screen (X and Y coordinates stay constant).
l_searchZ :: WorkspaceQuery -> WSType
l_searchZ q = WSIs $ do
  windowSet <- gets windowset
  let
    xzy = W.tag . W.workspace $ W.current windowSet
    y = l_YFrom xzy
    x = l_XFrom xzy
    predicate ww = l_resolveQuery q ww
      && l_XFrom (W.tag ww) == x
      && l_YFrom (W.tag ww) == y
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
l_searchZPreferZGroup :: ZGroup -> X XZY
l_searchZPreferZGroup zGroup = do
  windowSet <- gets windowset
  let
    xineramaCount = length $ W.screens windowSet
    xzyCurrent = W.tag $ W.workspace $ W.current windowSet
    (x, _, y) = l_CoordsFromWindowSet windowSet
    -- Get all XZYs at the current XCoord/YCoord combination that belong to
    -- zGroup.
    xzys = filter ((==x) . l_XFrom) $ l_XZYsFrom xineramaCount zGroup id y
    xzysOfGroup =
      [ (xzy, length $ W.integrate' stack)
      | (W.Workspace xzy _ stack) <- W.workspaces windowSet
      , elem xzy xzys
      ]
    xzyPicked
      | null xzysOfGroup = xzyCurrent
      | otherwise = fst . head $ customSort xzysOfGroup
    customSort = sortBy $ l_compareDimensions
      [ comparing snd
      , comparing fst
      ]
  return xzyPicked

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

l_queryEmptiness :: HasWindows -> WindowSpace -> Bool
l_queryEmptiness hasWindows ww = case hasWindows of
  Empty -> isNothing $ W.stack ww
  NonEmpty -> isJust $ W.stack ww

l_queryZGroupMemberships :: ZGroupMemberships -> WindowSpace -> Bool
l_queryZGroupMemberships zGroupMemberships ww = and $ map
  (\(zGroup, expectedResult) -> expectedResult == l_inGroup zGroup ww)
  zGroupMemberships

l_inGroup :: ZGroup -> WindowSpace -> Bool
l_inGroup zGroup ww = (l_ZCoordToGroup . l_ZFrom $ W.tag ww) == zGroup

-- If shifting was unsuccessful, don't try to view it. I.e., either we can shift
-- a window to another workspace and view it, or there is no window to shift to
-- begin with (and we do nothing).
l_shiftAndView :: XZY -> WindowSet -> WindowSet
l_shiftAndView xzy windowSet = W.view xzy (W.shift xzy windowSet)

-- Only perform the given action if the given test pases.
l_if :: X Bool -> X () -> X ()
l_if test action = do
  ok <- test
  when ok action

l_windowCountInCurrentWorkspaceExceeds :: Int -> X Bool
l_windowCountInCurrentWorkspaceExceeds n = do
  windowSet <- gets windowset
  let
    windowCount = length . W.integrate' . W.stack . W.workspace $ W.current windowSet
  return $ windowCount > n

l_workspaceIsEmpty :: XZY -> X Bool
l_workspaceIsEmpty xzy = do
  windowSet <- gets windowset
  return . isJust $ listToMaybe
    [ ww
    | ww <- W.workspaces windowSet
    , W.tag ww == xzy
    , isNothing $ W.stack ww
    ]

-- Terminals (using various different color themes).
l_term1, l_term2 :: String
l_term1 = "~/syscfg/script/sys/terms/wb.sh"
l_term2 = "~/syscfg/script/sys/terms/wblue.sh"

l_isUbuntu :: String -> Bool
l_isUbuntu givenHost = any (\ubuntuHost -> isPrefixOf ubuntuHost givenHost) ubuntuHosts
  where
  ubuntuHosts = ["enif"]

l_isPortraitMonitorLayout :: String -> Bool
l_isPortraitMonitorLayout givenHost = any (\portraitHost -> isPrefixOf portraitHost givenHost) portraitHosts
  where
  portraitHosts = ["k0", "enif"]

l_keyBindings :: String -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
l_keyBindings hostname conf@XConfig {XMonad.modMask = hypr} = M.fromList $
  -- Close focused window.
  [ ((hypr,   xK_d            ), kill)

  -- Rotate through the available layout algorithms. The TAB key is bound for
  -- better ergonomics for the ZQ layout (for the Esrille Nisse keyboard; see
  -- https://github.com/listx/new-keyboard).
  , ((hypr,   xK_space        ), sendMessage NextLayout)
  , ((hypr,   xK_Tab          ), sendMessage NextLayout)

  -- Reset the layouts on the current workspace to default.
  , ((hypr,   xK_q            ), setLayout $ XMonad.layoutHook conf)

  -- Move focus to the next/prev window.
  , ((hypr,   xK_j            ), windows W.focusDown)
  , ((hypr,   xK_k            ), windows W.focusUp)

  -- Swap the focused window with the next/prev window.
  , ((hyprS,  xK_j            ), windows W.swapDown)
  , ((hyprS,  xK_k            ), windows W.swapUp)

  -- Swap the focused window and the master window.
  , ((hypr,   xK_Return       ), windows W.swapMaster)

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

  -- Lock screen (Ubuntu only) or quit.
  , ((hypr,   xK_Escape       ), lockOrQuit)

  -- Toggle window borders.
  , ((hypr,   xK_b            ), withFocused toggleBorder)

  -- Move mouse away to bottom-right of currently focused window.
  , ((hypr,   xK_BackSpace    ), warpToWindow 1 1)

  -- Go to empty Workspace, on the current YCoord, in the current Xinerama
  -- screen. For shifting an existing window to an empty Workspace, only do so
  -- if there is indeed a window to work with in the current Workspace (i.e., if
  -- all ZCoords at this X/Y-coordinate pair is full, do nothing).
  , ((hypr,   xK_o            ), moveTo Next $ l_searchZ (WQ Empty []))
  , ((hyprS,  xK_o            ), l_if
      (l_windowCountInCurrentWorkspaceExceeds 0)
      (shiftTo Next $ l_searchZ (WQ Empty [])))
  ]
  ++
  -- Go to any non-empty Workspace only within the Z-Axis (X and Y remain
  -- untouched) The Z-axis direction is either Next or Prev (depending on the
  -- key). If we use the Shift key, move the current window to that direction,
  -- unless there is only 1 window.
  [ ((modifier, key), action)
  | (key, dir) <-
    [ (xK_n, Next)
    , (xK_p, Prev)
    ]
  , (modifier, action) <-
    [ (hypr, moveTo dir $ l_searchZ (WQ NonEmpty []))
    , (hyprS, l_if
        (l_windowCountInCurrentWorkspaceExceeds 0)
        ((\wst -> doTo dir wst getSortByIndex (windows . l_shiftAndView))
          =<< l_searchZPreferNonEmpty))
    ]
  ]
  ++
  -- NOTE: These bindings are rarely used any more, if at all. Consider
  -- deprecating them.
  -- hypr-[1..9, 0, F1-F10]: Switch to workspace N.
  -- hyprS-[1..9, 0, F1-F10]: Move focused window to workspace N.
  -- NOTE: Depending on the machine, we change the order of keys to optimize
  -- for the keyboard layout used on the machine. The order in
  -- `forQwertyKeyboard' is coincidentally optimized for that layout, because
  -- the "1", "2", "3" keys etc. are nearest the left side of the keyboard
  -- where we have our XMonad mod key (CapsLock remapped to Hyper key). In
  -- `forZQKeyboard', the middle finger of the numeric home row gets priority
  -- as the first VW because it is more ergonomic than the "1" key.
  [((hypr .|. mask, k         ), windows $ onCurrentScreen f i)
    | (i, k) <- zip (workspaces' conf) $ if l_isPortraitMonitorLayout hostname
      then forZQKeyboard
      else forQwertyKeyboard
    , (f, mask) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++
  -- H-{h,l}: Switch focus across X-axis (prev/next Xinerama screen).
  [((hypr, key), flip whenJust (windows . W.view) =<< screenWorkspace =<< sc)
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
  [((hyprS, key), l_if
    (l_windowCountInCurrentWorkspaceExceeds 0)
    (flip whenJust (windows . l_shiftAndView) =<< screenWorkspace =<< sc))
  | (key, sc) <- [(xK_h, screenBy (-1)), (xK_l, screenBy 1)]
  ]
  ++
  [ ((hyprA,  xK_j            ), l_viewYDir Next)
  , ((hyprAS, xK_j            ), l_if
                                  (l_windowCountInCurrentWorkspaceExceeds 0)
                                  (l_shiftY Next))
  , ((hyprA,  xK_k            ), l_viewYDir Prev)
  , ((hyprAS, xK_k            ), l_if
                                  (l_windowCountInCurrentWorkspaceExceeds 0)
                                  (l_shiftY Prev))
  , ((hyprA,  xK_n            ), l_viewYNonEmpty Next)
  , ((hyprA,  xK_p            ), l_viewYNonEmpty Prev)
  , ((hypr,   xK_t            ), l_viewLastY)
  ]
  ++
  -- Launch apps.
  [ ((hypr,   xK_i            ), spawn "qutebrowser")
  , ((hyprS,  xK_i            ), spawnSelected def [chromium, "firefox"])
  , ((hypr,   xK_e            ), spawn l_term1)
  -- Backup binding to launch a terminal in case our Hyper key (hypr) is
  -- unavailable. This happens whenever we unplug/replug our keyboard, and a
  -- terminal isn't already showing in a window somewhere to be able to call
  -- ~/syscfg/script/sys/initkeys.sh to re-initialize the Hyper key. Because the
  -- Hyper key is used exclusively to maneuver around Xmonad, we need a
  -- non-Hyper-key binding to launch a terminal to bootstrap ourselves back in
  -- with initkeys.sh.
  , ((altS,   xK_e            ), spawn l_term1)
  , ((hypr,   xK_u            ), spawn "emacs")
  ]
  where
  lockOrQuit
    | l_isUbuntu hostname = spawn "xscreensaver-command -lock"
    | otherwise = runSelectedAction def sessionActions
  sessionActions =
    [ ("Recompile/restart XMonad", spawn "xmonad --recompile && xmonad --restart")
    , ("Quit XMonad", io exitSuccess)
    ]
  shrinkExpand master slave = if l_isPortraitMonitorLayout hostname
    then sendMessage slave
    else sendMessage master
  chromium
    | l_isUbuntu hostname = "google-chrome"
    | otherwise = "chromium"
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
    [ xK_5
    , xK_6
    , xK_4
    , xK_2
    , xK_3
    , xK_1
    , xK_8
    , xK_7
    , xK_9
    , xK_0
    , xK_F1
    , xK_F2
    , xK_F3
    , xK_F4
    , xK_F5
    , xK_F6
    , xK_F7
    , xK_F8
    , xK_F9
    , xK_F10
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
    , xK_F1
    , xK_F2
    , xK_F3
    , xK_F4
    , xK_F5
    , xK_F6
    , xK_F7
    , xK_F8
    , xK_F9
    , xK_F10
    ]

l_mouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
l_mouseBindings XConfig {XMonad.modMask = hypr} = M.fromList
  -- hypr-button1 (left-click): Set the window to floating mode and move by
  -- dragging.
  [ ( (hypr, button1)
  , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
  )
  -- hypr-button2 (middle-click): Raise the window to the top of the stack.
  -- This binding is almost never used, but we keep it here for instructive
  -- purposes.
  , ((hypr, button2), \w -> focus w >> windows W.shiftMaster)
  ,
  -- hypr-button3 (right-click): Set the window to floating mode and resize by
  -- dragging.
  ( (hypr, button3)
  , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
  )
  -- You may also bind events to the mouse scroll wheel (button4 and button5).
  ]

-- Tall is the default tiling algorithm, which partitions the screen into two
-- panes. It takes three arguments x y z, where x is the default number of
-- windows in the master pane, y is the percent of the screen to increment by
-- when resizing panes, and z is the default proportion of the screen occupied
-- by the master pane.
l_layoutHook :: Choose (Mirror Tall) (Choose Tall (XLL.ModifiedLayout WithBorder Full)) Window
l_layoutHook = (Mirror $ tiled 1) ||| tiled 1 ||| noBorders Full
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
l_managementHook :: Int -> ManageHook
l_managementHook xineramaCount = composeOne $
  [ className =? "Gimp"               -?> doFloat
  , className =? "Agave"              -?> doCenterFloat
  , resource  =? "desktop_window"     -?> doIgnore
  , resource  =? "kdesktop"           -?> doIgnore
  , resource  =? "floatme"            -?> doCenterFloat
  -- Move browsers to the ZGNet ZGroup. If all ZCoords in ZGNet are full,
  , className =? "qutebrowser"        -?> doShift =<< toZGNet
  , className =? "Google-chrome"      -?> doShift =<< toZGNet
  , className =? "Chromium-browser"   -?> doShift =<< toZGNet
  , resource  =? "Navigator"          -?> doShift =<< toZGNet
  , className =? "Blender:Render"     -?> doFloat
  , resource  =? "Browser"            -?> doFloat
  , className =? "Xsane"              -?> doFloat
  , className =? "Spektra"            -?> doFloat
  , className =? "Glade"              -?> doFloat
  , className =? "Anki"               -?> doFloat
  , className =? "Qcp"                -?> doFloat
  , className =? "mupen64plus"        -?> doFloat
  ]
  ++
  -- This is useful for auto-moving a terminal screen we spawn elsewhere in
  -- this config file to a particular workspace.
  map
    (\xzy -> resource =? ("atWorkspace_" ++ xzy) -?> doShift xzy)
    (l_XZYs xineramaCount)
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
  -- Setup keyboard.
  spawn "~/syscfg/xmonad/xenv.sh"
  windowSet <- gets windowset
  spawn "qutebrowser"
  return ()
  y <- gets (l_YFromWindowSet . windowset)
  let
    xineramaCount = length $ W.screens windowSet
    -- Spawn rtorrent on the rightmost screen (XCoord index of -1; we use -1
    -- because we don't know how many screens there will actually be).
    rtorrent = spawn $ l_term2
      ++ " -name atWorkspace_"
      ++ l_XZYFrom (-1) xineramaCount ZGSys y
      ++ " -e rtorrent"
  -- Spawn one terminal in every screen at the "Work" ZGroup at the current
  -- YCoord (but only if that screen is empty). We have to feed in `(take 1)' in
  -- order to spawn terminals in a single ZCoord.
  mapM_
    (\xzy -> l_if (l_workspaceIsEmpty xzy) (spawn $ l_term1 ++ " -name atWorkspace_" ++ xzy))
    $ l_XZYsFrom xineramaCount ZGWork (take 1) y
  -- Spawn htop on the rightmost screen.
  spawn $ l_term1
    ++ " -name atWorkspace_"
    ++ l_XZYFrom (-1) xineramaCount ZGSys y
    ++ " -e htop"
  spawn "emacs --daemon"
  when (elem hostname ["k0"]) rtorrent

main :: IO ()
main = do
  xineramaCount <- countScreens
  hostname <- fmap nodeName getSystemID
  if l_isPortraitMonitorLayout hostname
    then xmonad (myconf hostname xineramaCount) {layoutHook = l_layoutNoMirror}
    else xmonad $ myconf hostname xineramaCount
  where
  myconf hostname xineramaCount = def
    { terminal           = "urxvt"
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
    , workspaces         = withScreens (fromIntegral xineramaCount) $ map fst l_ZYGroups
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#ffffff"
    , keys               = l_keyBindings hostname
    , mouseBindings      = l_mouseBindings
    , layoutHook         = l_layoutHook
    , manageHook         = l_managementHook xineramaCount
    , handleEventHook    = mempty
    , logHook            = mempty
    , startupHook        = l_startupHook hostname
    }
