-- required imports
import XMonad
import Data.Monoid
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- optional imports
import Data.Maybe (isJust, isNothing)
import XMonad.Actions.Warp -- for mouse warping
import XMonad.Actions.GridSelect -- for selecting workspaces, etc. from a 2D, pretty grid
import XMonad.Actions.NoBorders -- for toggling borders on a per-window basis
import XMonad.Actions.CycleWS -- for workspace-to-workspace navigation
import XMonad.Hooks.ManageHelpers -- for doCenterFloat

import XMonad.Layout.NoBorders -- enable borderless layouts
import XMonad.Layout.Circle -- circle layout
import XMonad.Layout.LayoutHints -- for (among other things) removing GVim's dead borders automatically

import XMonad.Util.WorkspaceCompare (getSortByIndex)
import System.Posix.Unistd -- for getting hostname
import XMonad.Hooks.EwmhDesktops -- for _NET_WINDOW_WINDOW (emacs + SCIM bridge)

import System.Random
import Data.Array.IO
import Control.Monad

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvt"

myFocusFollowsMouse = True
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
-- You can double-check which key is which modmask by running 'xmodmap'
myModMask       = mod3Mask -- use the CAPSLOCK key
altMask         = mod1Mask -- alias "altMask" for left alt key

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask

data MyWSGroup = Work | Net | Misc | Music | Net2 | Sys
    deriving (Eq, Ord, Enum, Show)

_MYWSGROUPS = [Net ..] -- list of all MyWSGroup

-- Use an association list for more flexible custom workspace manipulation.
--
-- The fact that we do not necessarily follow the ordering of the MYWSGroup data type allows for
-- maximum flexibility: we can either directly choose, via xK_1 ... xK_F12 keys what we want (like
-- we did in the past), but at the same time we can also choose to navigate via the ordering in
-- MyWSGroup via the custom diffGrpWS function.
myWorkspaceGroups :: [(WorkspaceId, MyWSGroup)]
myWorkspaceGroups = [ ("1",   Work)
                    , ("2",   Work)
                    , ("3",   Work)
                    , ("4",   Work)
                    , ("5",   Work)
                    , ("6",   Work)
                    , ("7",   Work)
                    , ("8",   Net)
                    , ("9",   Net)
                    , ("0",   Music) -- ncmpcpp, terminal named "mplayer" (used when streaming music on LAN)
                    , ("F1",  Misc)
                    , ("F2",  Misc)
                    , ("F3",  Misc)
                    , ("F4",  Misc)
                    , ("F5",  Misc)
                    , ("F6",  Misc)
                    , ("F7",  Misc)
                    , ("F8",  Misc)
                    , ("F9",  Net2) -- IRC
                    , ("F10", Net2)
                    , ("F11", Sys)
                    , ("F12", Sys)
                    ]

myWorkspaces = map fst myWorkspaceGroups -- 1..9, 0, F1-F12

myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#ffffff"

-- terminals (various different color themes)
term1 = "~/syscfg/script/sys/terms/wb.sh"
term2 = "~/syscfg/script/sys/terms/bw.sh"
term3 = "~/syscfg/script/sys/terms/wB.sh"

orgIntraday = " -name floatme -e ~/prog/timeflux/src/term.sh intraday ~/org/life.org @@@"
orgPlans5w = " -name floatme -e ~/prog/timeflux/src/term.sh plans5w ~/org/life.org @@@"
orgGoals10w = " -name floatme -e ~/prog/timeflux/src/term.sh goals10w ~/org/life.org @@@"
orgLeftToday = " -name floatme -e ~/prog/timeflux/src/term.sh lefttoday ~/org/life.org @@@"

myKeys :: String -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys hostname conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- close focused window
    , ((modm,               xK_d     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_w     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_e     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modm .|. controlMask, xK_Escape     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm .|. altMask,   xK_Escape     ), spawn "xmonad --recompile; xmonad --restart")

    -- CUSTOM KEYBINDINGS HERE --
    , ((mod4Mask              , xK_1     ), spawn (term1 ++ orgIntraday))
    , ((mod4Mask              , xK_2     ), spawn (term1 ++ orgPlans5w))
    , ((mod4Mask              , xK_3     ), spawn (term1 ++ orgGoals10w))
    , ((mod4Mask              , xK_9     ), spawn (term1 ++ orgLeftToday))
    , ((mod4Mask              , xK_0     ), spawn "emacsclient -c /home/listdata/org/life.org")
    , ((mod4Mask              , xK_c     ), spawn "galculator")
    , ((mod4Mask .|. shiftMask, xK_c     ), spawn "gcalctool")
    , ((mod4Mask              , xK_d     ), spawn "amule")
    , ((mod4Mask              , xK_e     ), spawn (term1 ++ " -e mutt"))
    , ((mod4Mask .|. shiftMask, xK_f     ), spawn "gnucash")
    , ((mod4Mask              , xK_h     ), spawn (term2 ++ " -e ncmpcpp"))
    , ((mod4Mask .|. shiftMask, xK_h     ), spawn (term2 ++ " -e mplayer -ao alsa -softvol -volume 100 -volstep 1 -demuxer ogg -prefer-ipv4 http://192.168.0.110:8000/mpd.ogg"))
    , ((mod4Mask              , xK_i     ), spawn "gimp")
    , ((mod4Mask              , xK_m     ), spawn "blender")
    , ((mod4Mask              , xK_n     ), spawn "firefox")
    , ((mod4Mask .|. shiftMask, xK_n     ), io sitesRand >>= spawn . ("firefox " ++))
    , ((mod4Mask              , xK_w     ), spawn "soffice")
    , ((mod4Mask              , xK_x     ), spawn term1)
    , ((mod4Mask .|. shiftMask, xK_x     ), spawn term3)
    , ((mod4Mask              , xK_z     ), spawn term2)
    -- ncmpcpp (mpd) controls
    , ((mod4Mask .|. controlMask, xK_i                   ), spawn "ncmpcpp stop")
    , ((mod4Mask .|. controlMask .|. shiftMask, xK_i     ), spawn "ncmpcpp stop; ncmpcpp play") -- "reset" current song to beginning
    , ((mod4Mask .|. controlMask, xK_o                   ), spawn "ncmpcpp toggle")
    , ((mod4Mask .|. controlMask, xK_h                   ), spawn $ mpcSeek hostname (-4))
    , ((mod4Mask .|. controlMask .|. shiftMask, xK_h     ), spawn $ mpcSeek hostname (-16))
    , ((mod4Mask .|. controlMask, xK_j                   ), spawn "ncmpcpp next")
    , ((mod4Mask .|. controlMask, xK_k                   ), spawn "ncmpcpp prev")
    , ((mod4Mask .|. controlMask, xK_l                   ), spawn $ mpcSeek hostname 4)
    , ((mod4Mask .|. controlMask .|. shiftMask, xK_l     ), spawn $ mpcSeek hostname 16)
    , ((mod4Mask .|. controlMask, xK_semicolon           ), spawn "ncmpcpp play")
    -- change keyboard layouts
    , ((mod4Mask              , xK_Escape), spawn "/home/listdata/syscfg/script/sys/layout_toggle.sh")
    -- toggle borders
    , ((modm              , xK_b ),   withFocused toggleBorder)
    -- gridselect
    , ((modm              , xK_f), goToSelected defaultGSConfig)
    -- volume controls
    , ((modm              , xK_backslash ), spawn "amixer -q set Master toggle")
    , ((modm              , xK_minus     ), spawn "amixer -q set Master 1dB- unmute")
    , ((modm              , xK_equal     ), spawn "amixer -q set Master 1.5dB+ unmute")
    -- screen brightness toggle
    , ((modm .|. shiftMask, xK_backslash ), spawn "sudo brightness") -- toggle brightness (100% or 0%)
    , ((modm .|. shiftMask, xK_minus ), cpufreqSet "powersave" hostname)
    , ((modm .|. shiftMask, xK_equal ), cpufreqSet "ondemand" hostname)
    , ((modm .|. shiftMask, xK_BackSpace ), cpufreqSet "performance" hostname)
    -- move mouse away to bottom-right of currently focused window
    , ((modm              , xK_BackSpace), warpToWindow 1 1)
    ]
    ++

    -- mod-[1..9, 0, F1-F12], Switch to workspace N
    -- mod-shift-[1..9, 0, F1-F12], Move focused window to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1..xK_9] ++ [xK_0] ++ [xK_F1..xK_F11] ++ [xK_F12])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    [
      ((modm .|. altMask,   xK_j),              moveTo Next nonEmptyGrpWS) -- go to next non-empty WS, in current Group
    , ((modm .|. altMask,   xK_k),              moveTo Prev nonEmptyGrpWS) -- go to prev non-empty WS, in current Group
    , ((modm .|. altMask,   xK_l),              moveTo Next $ diffGrpWS 1)     -- go to next WS Group
    , ((modm .|. altMask,   xK_h),              moveTo Next $ diffGrpWS (-1))  -- go to previous WS Group (the "moveTo Next" part here ensures that, after getting our desired new WSGroup, that we always display the top WS of that Group
    , ((modm .|. altMask,   xK_o),              action_o)    -- go to next hidden empty WS (if possible), in current Group, and spawn a new "window" there (depends on group)
    , ((modm, xK_n),                            moveTo Next $ nonEmptyWSExceptGrps [Net2, Sys])    -- go to any non-empty WS, except those WS belonging to the given WS Groups (for making sure that our "desk" is clean before we log off/shutdown)
    , ((modm .|. altMask .|. shiftMask, xK_j),  shiftTo Next grpWS) -- move window to next WS, in current Group
    , ((modm .|. altMask .|. shiftMask, xK_k),  shiftTo Prev grpWS) -- move window to prev WS, in current Group
    , ((modm .|. altMask .|. shiftMask, xK_l),  shiftTo Next $ diffGrpWS 1) -- move window to next WS group
    , ((modm .|. altMask .|. shiftMask, xK_h),  shiftTo Next $ diffGrpWS (-1)) -- move window to prev WS group
    , ((modm, xK_semicolon),                    toggleWS) -- go to WS displayed previously
    , ((modm .|. altMask, xK_BackSpace),        resetScreensToWSTops) -- reset each Xinerama screen so that they point to the heads of each group
    ]

    ++

    --
    -- mod-{h,l}, Switch to prev/next Xinerama screens
    -- mod-shift-{h,l}, Move client to prev/next screen
    --
    [((m .|. modm, key), (screenWorkspace =<< sc) >>= flip whenJust (windows . f))
        | (key, sc) <- [(xK_h, screenBy (-1)),(xK_l, screenBy 1)]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

mpcSeek :: String -> Int -> String
mpcSeek hostname sec = "mpc -h 192.168.0.110 -p " ++ port ++ " seek " ++ show' sec
    where
        port = case hostname of
            "exelion" -> "6600" -- alsa
            _ -> "6601" -- icecast
        show' n
            | n < 0 = show n
            | otherwise = '+':show n

cpufreqSet :: String -> String -> X ()
cpufreqSet governor hostname = case hostname of
    "exelion"   -> mapM_ (spawn . cpu) [0..3]
    "aether"    -> mapM_ (spawn . cpu) [0..1]
    "luxion"    -> mapM_ (spawn . cpu) [0]
    _ -> return ()
    where
        cpu n = "sudo cpufreq-set -c " ++ show n ++ " -g " ++ governor

-- since CycleWS does not export this useful function, we have to copy/paste it in here...
screenBy :: Int -> X (ScreenId)
screenBy d = do ws <- gets windowset
                let now = W.screen (W.current ws)
                return $ (now + fromIntegral d) `mod` fromIntegral (length (W.screens ws))

-- Depending on the current MyWSGroup, open up different new "windows" that make sense for that
-- workspace group.
action_o :: X ()
action_o =
    do  {
        ; currentWS <- gets (W.currentTag . windowset)
        ; let currentWSGrp = getWSGroup currentWS
        -- first try to move to the next empty WS in the group
        ; moveTo Next emptyGrpWS
        -- now do a specific action that suits this WS group
        ; case currentWSGrp of
            Net         -> spawn "firefox"
            otherwise   -> spawn term1
        }

-- An empty WS belonging to the same group as the current one.
emptyGrpWS :: WSType
emptyGrpWS =
    WSIs $ do
    { currentWS <- gets (W.currentTag . windowset)
    ; let currentWSGrp = getWSGroup currentWS
    ; let isEmpty = isNothing . W.stack
    ; let isCurrentWSGrp = isWSGroup currentWSGrp . W.tag
    ; return (\w -> isEmpty w && isCurrentWSGrp w)
    }

-- A non-empty WS belonging to the same group as the current one.
nonEmptyGrpWS :: WSType
nonEmptyGrpWS =
    WSIs $ do {
      currentWS <- gets (W.currentTag . windowset)
    ; let currentWSGrp = getWSGroup currentWS
    ; let isNonEmpty = isJust . W.stack
    ; let isCurrentWSGrp = isWSGroup currentWSGrp . W.tag
    ; return (\w -> isNonEmpty w && isCurrentWSGrp w)
    }

-- An empty WS belonging to the given group.
emptyWSGrp :: MyWSGroup -> WSType
emptyWSGrp grp =
    WSIs $ do {
      let isEmpty = isNothing . W.stack
    ; let isMemberOfGivenGrp = isWSGroup grp . W.tag
    ; return (\w -> isEmpty w && isMemberOfGivenGrp w)
    }

-- A non-empty WS belonging to any group except those in the given group list.
nonEmptyWSExceptGrps :: [MyWSGroup] -> WSType
nonEmptyWSExceptGrps grps =
    WSIs $ do {
      let isNonEmpty = isJust . W.stack
    ; let isMemberOfGivenGrps = isWSGroups grps . W.tag
    ; return (\w -> isNonEmpty w && not (isMemberOfGivenGrps w))
    }

-- A WS belonging to a different group (next/prev group) than the current one.
diffGrpWS :: Int -> WSType
diffGrpWS dir =
    WSIs $ do {
      currentWS <- gets (W.currentTag . windowset)
    ; let currentWSGrp = getWSGroup currentWS
    ; do    { let diffGrp = if dir > 0
                                then nextGrp currentWSGrp
                                else prevGrp currentWSGrp
            ; return (\w -> isWSGroup diffGrp . W.tag $ w)
            }
    }

-- Any WS in the current group.
grpWS :: WSType
grpWS =
    WSIs $ do {
      currentWS <- gets (W.currentTag . windowset)
    ; let currentWSGrp = getWSGroup currentWS
    ; return (\w -> isWSGroup currentWSGrp . W.tag $ w)
    }

-- The next or previous group of given group. Will cycle back to the first group if the given one is
-- the last group.
nextGrp, prevGrp :: MyWSGroup -> MyWSGroup
nextGrp g = if g == last _MYWSGROUPS
                then head _MYWSGROUPS
                else head [x | x <- _MYWSGROUPS, x > g]

prevGrp g = if g == head _MYWSGROUPS
                then last _MYWSGROUPS
                else last [x | x <- _MYWSGROUPS, x < g]

getWSids :: MyWSGroup -> [WorkspaceId]
getWSids g = map fst $ filter (\(x,y) -> y == g) myWorkspaceGroups

getWSGroup :: WorkspaceId -> MyWSGroup
getWSGroup wsid = head . map snd $ filter (\(x,y) -> x == wsid) myWorkspaceGroups

-- Confirms if a given WorkspaceId str is part of a given MyWSGroup g.
--
-- The order of the arguments is "backwards" because we compose this function with StackSet's tag
-- function, which returns the WorkspaceId, e.g., "(\w -> isWSGroup Net . W.tag $ w)".
isWSGroup :: MyWSGroup -> WorkspaceId -> Bool
isWSGroup grp str
    | lookup str myWorkspaceGroups == Just grp = True
    | otherwise = False

-- Same as isWSGroup, but takes a list of valid MyWSGroups instead of just a single MyWSGroup.
isWSGroups :: [MyWSGroup] -> WorkspaceId -> Bool
isWSGroups grps str
    | lookup str myWorkspaceGroups' /= Nothing = True
    | otherwise = False
    where myWorkspaceGroups' = filter (\(x,y) -> any (==y) grps) myWorkspaceGroups

-- same as CycleWS's moveTo, but w/ view instead of greedyView
moveToNogreed :: Direction1D -> WSType -> X ()
moveToNogreed dir t = findWorkspace getSortByIndex dir t 1 >>= windows . W.view

-- Try to find an empty WSID belonging to the given group, and return its
-- name.  If group is full, then return the current WSID.
tryGetEmptyWSIDofGroup :: MyWSGroup -> X WorkspaceId
tryGetEmptyWSIDofGroup g =
    do  {
        ; ws0 <- findWorkspace getSortByIndex Next (emptyWSGrp g) 0 -- an empty WS of group g 0 WSs away from current WS
        ; ws1 <- findWorkspace getSortByIndex Next (emptyWSGrp g) 1 -- an empty WS of group g 1 WSs away from current WS
        ; if isWSGroup g ws0
            then return ws0 -- if current WS also happens to be an empty WS of the group g, then return this WSID
            else return ws1 -- the empty WS of group g, NOT counting the current WS; if such a WS does not exist, then ws1 is just the current WS
        }

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myLayout = Mirror tiled
        ||| tiled
        ||| noBorders Full
        ||| layoutHints Circle
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled   = Tall nmaster delta ratio

        -- The default number of windows in the master pane
        nmaster = 1

        -- Default proportion of screen occupied by master pane
        ratio   = 1/2

        -- Percent of screen to increment by when resizing panes
        delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- e.g. WM_CLASS(STRING) = "Qt-subapplication", "VirtualBox"
-- here, Qt-subapplication is resource, and
-- VirtualBox is className
--
myManageHook :: ManageHook
myManageHook = composeAll $
    [ className =? "MPlayer"                            --> doFloat
    , className =? "Gimp"                               --> doFloat
    , className =? "Agave"                              --> doCenterFloat
    , resource  =? "desktop_window"                     --> doIgnore
    , resource  =? "kdesktop"                           --> doIgnore
    , resource  =? "vlc"                                --> doCenterFloat
    , resource  =? "WeeChat"                            --> doShift "F9"
    , stringProperty "WM_NAME"  =? "rtorrent"           --> doShift "F10"
    , stringProperty "WM_NAME"  =? "htop"               --> doShift "F11"
    , stringProperty "WM_ICON_NAME"  =? "iftop"         --> doShift "F12"
    , stringProperty "WM_NAME"  =? "alsamixer"          --> doShift "F12"
    , resource  =? "floatme"                            --> doCenterFloat
    , stringProperty "WM_NAME"  =? "ncmpcpp"            --> doShift =<< liftX (tryGetEmptyWSIDofGroup Music)
    , stringProperty "WM_NAME"  =? "mplayer"            --> doShift =<< liftX (tryGetEmptyWSIDofGroup Music)
    , resource  =? "Navigator"                          --> doShift =<< liftX (tryGetEmptyWSIDofGroup Net)
    , className  =? "Evince"                            --> doShift =<< liftX (tryGetEmptyWSIDofGroup Misc)
    , className  =? "Blender:Render"                    --> doFloat
    , resource  =? "Browser"                            --> doFloat
    , className  =? "Galculator"                        --> doCenterFloat
    , className  =? "Gcalctool"                         --> doCenterFloat
    , className  =? "XClock"                            --> doCenterFloat
    , className  =? "Audacity"                          --> doFloat
    , className  =? "Gitk"                              --> doCenterFloat
    , className  =? "XDvi"                              --> doCenterFloat
    , className  =? "Scid"                              --> doFloat
    , stringProperty "WM_NAME"  =? "Scid"               --> doFloat
    , className  =? "Toplevel"                          --> doFloat -- Scid's many popup windows
    , className  =? "Pychess"                           --> doFloat
    , className  =? "Glchess"                           --> doFloat
    , className  =? "Raptor"                            --> doFloat
    , className  =? "Smplayer"                          --> doFloat
    , className  =? "linux_client"                      --> doFloat
    , className  =? "Bsnes"                             --> doCenterFloat
    , className  =? "Phoenix"                           --> doCenterFloat
    , className  =? "VirtualBox"                        --> doFloat
    , className  =? "libreoffice-writer"                --> doFloat
    , className  =? "Xsane"                             --> doFloat
    ]
    ++  [ resource =? ("atWorkspace" ++ s) --> doShift s
        | s <- map show [0..9] ++ map (('F':) . show) [1..12]
        ]

myEventHook = mempty
myLogHook = return ()

myStartupHook :: String -> X ()
myStartupHook hostname =
    do  {
        ; spawnIfGrpTopWSNotFull Net "firefox"
        ; spawnIfGrpNotFull Work $ term1 ++ " -name atWorkspace1"
        ; spawn $ term1 ++ orgIntraday
        ; spawnIfGrpNotFull Sys $ term1 ++ " -e alsamixer"
        ; spawnIfGrpNotFull Sys $ term2 ++ " -n iftop -e sudo iftop -B -i eth" ++ (if hostname == "exelion" then "1" else "0")
        ; spawnIfGrpNotFull Sys $ term1 ++ " -e htop"
        ; case hostname of
            "exelion" -> do { spawnIfGrpTopWSNotFull Music $ term2 ++ " -e ncmpcpp"
                            ; spawnIfGrpNotFull Net2 $ term3 ++ " -e rtorrent"
                            }
            "aether" -> do  { spawnIfGrpNotFull Net2 $ term3 ++ " -e rtorrent"
                            }
            _ -> return ()
        -- ; case hostname of
        --     "exelion" -> resetScreensToWSTops
        --     _ -> return ()
        }

-- reset all xinerama screens to point to top WS of each group
resetScreensToWSTops :: X ()
resetScreensToWSTops = gets (W.screens . windowset) >>= myloop . length
    where myloop :: Int -> X ()
          myloop ss = recurse (S 0) (S (ss - 1))
            where   recurse :: ScreenId -> ScreenId -> X ()
                    recurse acc until = if acc <= until
                                            then do {
                                                    ; screenWorkspace acc >>= flip whenJust (windows . W.view)
                                                    ; windows $ W.greedyView (head (getWSids (_MYWSGROUPS!!(toint acc))))
                                                    ; recurse (acc + 1) until
                                                    }
                                            else return ()
                        where toint (S a) = a

spawnIfGrpNotFull :: MyWSGroup -> String -> X ()
spawnIfGrpNotFull g command =
    do  {
        ; wsid <- tryGetEmptyWSIDofGroup g
        ; if isWSGroup g wsid
            then spawn command
            else return ()
        }

spawnIfGrpTopWSNotFull :: MyWSGroup -> String -> X ()
spawnIfGrpTopWSNotFull g command =
    do  {
        ; wsid <- tryGetEmptyWSIDofGroup g
        ; if (isWSGroup g wsid && (wsid == head (getWSids . getWSGroup $ wsid)))
            then spawn command
            else return ()
        }

-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
    where
        n = length xs
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray n xs =  newListArray (1,n) xs

sites :: [String]
sites =
    -- news
    [ "www.lemonde.fr"
    , "www.zeit.de"
    , "www.nikkei.com"
    , "www.hani.co.kr"
    , "www.sfgate.com"
    -- hobbies
    , "www.chessbase.com"
    , "http://www.reddit.com/r/programming/top?t=week"
    , "http://news.ycombinator.com/best"
    , "www.linuxfr.org"
    ]

sitesRand :: IO String
sitesRand = shuffle sites >>= return . unwords

main = do
    hostname <- fmap nodeName getSystemID
    xmonad $ ewmh defaultConfig
        { terminal           = myTerminal
        , focusFollowsMouse  = myFocusFollowsMouse
        , borderWidth        = myBorderWidth
        , modMask            = myModMask
        , numlockMask        = myNumlockMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , keys               = myKeys hostname
        , mouseBindings      = myMouseBindings
        , layoutHook         = myLayout
        , manageHook         = myManageHook
        , handleEventHook    = myEventHook
        , logHook            = myLogHook
        , startupHook        = myStartupHook hostname
        }

-- vim:syntax=haskell
