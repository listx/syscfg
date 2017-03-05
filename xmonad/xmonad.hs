module Main where

import Control.Monad
import Data.Array.IO
import Data.Maybe
	( fromMaybe
	, isJust
	, isNothing
	)
import System.Exit
import System.Posix.Unistd
	( nodeName
	, getSystemID
	)
import XMonad
import XMonad.Actions.CycleWS
	( Direction1D
		( Next
		, Prev)
	, WSType (WSIs)
	, findWorkspace
	, moveTo
	, screenBy
	, shiftTo
	, toggleWS
	)
import XMonad.Actions.GridSelect
	( defaultGSConfig
	, goToSelected
	, runSelectedAction
	, spawnSelected
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
import XMonad.Util.WorkspaceCompare
	( getSortByIndex )
import qualified Data.Map as M
import qualified XMonad.Layout.LayoutModifier as XLL
import qualified XMonad.StackSet as W

isUbuntu :: String -> Bool
isUbuntu = flip elem ["larver-w0", "larver-w1"]

isPersonal :: String -> Bool
isPersonal = flip elem ["k0", "k1"]

isPortraitMonitorLayout :: String -> Bool
isPortraitMonitorLayout = flip elem ["k0", "larver-w0"]

data MyVWGroup
	= Work
	| Net
	| Misc
	| Sys
	deriving (Eq, Ord, Enum, Show)

-- Use an association list for more flexible custom workspace manipulation. We
-- basically just attach some more data to a VirtualWorkspace.
myWorkspaceGroups :: [(VirtualWorkspace, MyVWGroup)]
myWorkspaceGroups =
	[ ("a", Work)
	, ("b", Work)
	, ("c", Work)
	, ("d", Work)
	, ("e", Work)
	, ("f", Work)
	, ("g", Net)
	, ("h", Net)
	, ("i", Net)
	, ("j", Sys)
	, ("k", Misc)
	, ("l", Misc)
	, ("m", Misc)
	, ("n", Misc)
	, ("o", Misc)
	, ("p", Misc)
	, ("q", Misc)
	, ("r", Misc)
	, ("s", Misc)
	, ("t", Misc)
	]

-- Terminals (using various different color themes).
term1, term2 :: String
term1 = "~/syscfg/script/sys/terms/wb.sh"
term2 = "~/syscfg/script/sys/terms/wblue.sh"

-- Key bindings. Roughly, modm (XMonad key) deals with general window-management
-- features, while supr (super key) deals with launching applications.
myKeys :: String -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys hostname conf@XConfig {XMonad.modMask = modm} = M.fromList $
	-- Close focused window.
	[ ((modm,   xK_d            ), kill)

	-- Rotate through the available layout algorithms. The TAB key is bound for
	-- better ergonomics for the ZQ layout (for the Esrille Nisse keyboard; see
	-- https://github.com/listx/new-keyboard).
	, ((modm,   xK_space        ), sendMessage NextLayout)
	, ((modm,   xK_Tab          ), sendMessage NextLayout)

	-- Reset the layouts on the current workspace to default.
	, ((modm,   xK_q            ), setLayout $ XMonad.layoutHook conf)

	-- Move focus to the next/prev window.
	, ((modm,   xK_j            ), windows W.focusDown)
	, ((modm,   xK_k            ), windows W.focusUp)

	-- Swap the focused window with the next/prev window.
	, ((modmS,  xK_j            ), windows W.swapDown)
	, ((modmS,  xK_k            ), windows W.swapUp)

	-- Swap the focused window and the master window.
	, ((modm,   xK_Return       ), windows W.swapMaster)

	-- Unfloat/float window.
	, ((modm,   xK_f            ), withFocused $ windows . W.sink)
	, ((modmS,  xK_f            ), withFocused $ windows . flip W.float relativeDimenions)

	-- Shrink/Expand work on the Master window; the Mirror* counterparts do the
	-- same (although, from the looks of it, the definitions are somehow
	-- *reversed*), but for a slave window.
	, ((modmS,  xK_bracketleft  ), shrinkExpand Shrink MirrorExpand)
	, ((modmS,  xK_bracketright ), shrinkExpand Expand MirrorShrink)

	-- Increment/decrement the number of windows in the master area.
	, ((modm,   xK_m            ), sendMessage (IncMasterN 1))
	, ((modmS,  xK_m            ), sendMessage (IncMasterN (-1)))

	-- Go to any non-empty VW, except those VW belonging to the given VW Groups
	-- (for making sure that our "desk" is clean before we log off/shutdown).
	, ((modm,   xK_n            ), moveTo Next $ nonEmptyVWExceptGrps [Sys] False)
	, ((modmS,  xK_n            ), shiftTo Next $ nonEmptyVWExceptGrps [Sys] False)
	, ((modmAS, xK_n            ), moveTo Next $ nonEmptyVWExceptGrps [Sys] True)

	-- Go to empty VW. If all VWs in this screen are full, then do nothing.
	, ((modm,   xK_o            ), moveTo Next emptyVW)
	, ((modmS,  xK_o            ), shiftTo Next emptyVW)

	-- Go to VW displayed previously.
	, ((modm,   xK_t            ), toggleWS)

	-- View all windows as a grid.
	, ((modm,   xK_g            ), goToSelected defaultGSConfig)

	-- Lock screen (Ubuntu only) or quit.
	, ((modm,   xK_Escape       ), lockOrQuit)

	-- Toggle window borders.
	, ((modm,   xK_b            ), withFocused toggleBorder)

	-- Move mouse away to bottom-right of currently focused window.
	, ((modm,   xK_BackSpace    ), warpToWindow 1 1)
	]
	++
	-- modm-[1..9, 0, F1-F10]: Switch to workspace N.
	-- modmS-[1..9, 0, F1-F10]: Move focused window to workspace N.
	-- NOTE: Depending on the machine, we change the order of keys to optimize
	-- for the keyboard layout used on the machine. The order in
	-- `forQwertyKeyboard' is coincidentally optimized for that layout, because
	-- the "1", "2", "3" keys etc. are nearest the left side of the keyboard
	-- where we have our XMonad mod key (CapsLock remapped to Hyper key). In
	-- `forZQKeyboard', the middle finger of the numeric home row gets priority
	-- as the first VW because it is more ergonomic than the "1" key.
	[((modm .|. mask, k         ), windows $ onCurrentScreen f i)
		| (i, k) <- zip (workspaces' conf) $ if isPortraitMonitorLayout hostname
			then forZQKeyboard
			else forQwertyKeyboard
		, (f, mask) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
	++
	-- modm-{h,l}: Switch to prev/next Xinerama screens.
	-- modS-{h,l}: Move client to prev/next screen.
	[((m .|. modm, key          ), flip whenJust (windows . f) =<< screenWorkspace =<< sc)
		| (key, sc) <- [(xK_h, screenBy (-1)),(xK_l, screenBy 1)]
		, (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
	++
	-- Launch apps.
	[ ((supr,   xK_e            ), spawn "emacs")
	, ((supr,   xK_i            ), spawnSelected defaultGSConfig ["gimp", "blender"])
	, ((supr,   xK_n            ), spawn "qutebrowser")
	, ((suprS,  xK_n            ), spawnSelected defaultGSConfig ["firefox", chromium])
	, ((supr,   xK_x            ), spawn term1)
	, ((suprS,  xK_x            ), spawn term2)
	]
	where
	lockOrQuit
		| isUbuntu hostname = spawn "gnome-screensaver-command --lock"
		| otherwise = runSelectedAction defaultGSConfig sessionActions
	sessionActions =
		[ ("Recompile/restart XMonad", spawn "xmonad --recompile && xmonad --restart")
		, ("Quit XMonad", io exitSuccess)
		]
	shrinkExpand master slave = if isPortraitMonitorLayout hostname
		then sendMessage slave
		else sendMessage master
	chromium
		| isUbuntu hostname = "chromium-browser"
		| otherwise = "chromium"
	-- Alias "altMask" for left alt key.
	altMask = mod1Mask
	modmS = modm .|. shiftMask
	modmAS = modm .|. shiftMask .|. altMask
	supr = mod4Mask
	suprS = supr .|. shiftMask
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

-- An empty VW _in the current screen_.
emptyVW :: WSType
emptyVW = WSIs $ do
	(S currentScreen) <- gets (W.screen . W.current . windowset)
	return $ \w
		-> isEmpty w
		&& show currentScreen == takeWhile (/='_') (W.tag w)

-- An empty VW belonging to the given group, _in the current screen_.
emptyVWinGrp :: MyVWGroup -> WSType
emptyVWinGrp grp = WSIs $ do
	(S currentScreen) <- gets (W.screen . W.current . windowset)
	let
		isMemberOfGivenGrp = flip isVWinGroups [grp] . getVW
	return $ \w
		-> isEmpty w
		&& isMemberOfGivenGrp w
		&& show currentScreen == takeWhile (/='_') (W.tag w)

-- A non-empty VW belonging to any group except those in the given group list.
nonEmptyVWExceptGrps :: [MyVWGroup] -> Bool -> WSType
nonEmptyVWExceptGrps grps acrossScreens = WSIs $ do
	(S currentScreen) <- gets (W.screen . W.current . windowset)
	let
		nonEmptyExceptGrps w = all ($ w) [isNonEmpty, isNotMemberOfGivenGrps]
		isNotMemberOfGivenGrps = not . flip isVWinGroups grps . getVW
	return $ if acrossScreens
		then nonEmptyExceptGrps
		else \w
			-> nonEmptyExceptGrps w
			&& show currentScreen == takeWhile (/='_') (W.tag w)

-- Because of IndependentScreens, tags take the form of
-- "<ScreenId>_<VirtualWorkspace>". So to get to just the
-- VirtualWorkspace (VW), we have to drop all leading characters up to
-- the underscore. As there is no "dropWhileUpto" function in Haskell,
-- we emulate it with reverse+takeWhile. The caveat here is that our VWs
-- must not have an underscore in them.
getVW :: W.Workspace String l a -> String
getVW = removePWtag . W.tag

removePWtag :: String -> String
removePWtag = reverse . takeWhile (/='_') . reverse

isEmpty :: W.Workspace i l a -> Bool
isEmpty = isNothing . W.stack

isNonEmpty :: W.Workspace i l a -> Bool
isNonEmpty = isJust . W.stack

getVWsOfGroup :: MyVWGroup -> [VirtualWorkspace]
getVWsOfGroup g
	= map fst
	$ filter ((== g) . snd) myWorkspaceGroups

-- If we cannot find the group for this VW, default to "Work" group. A VW always
-- carries around with it the physical screen info as a prefix, so we have to
-- drop this prefix when we're doing lookups into `myWorkspaceGroups'.
getGroupOfVW :: VirtualWorkspace -> MyVWGroup
getGroupOfVW vw = fromMaybe Work $ lookup vw myWorkspaceGroups

-- Check if given VW belongs to any one of the given vw groups.
isVWinGroups :: VirtualWorkspace -> [MyVWGroup] -> Bool
isVWinGroups vw = elem (getGroupOfVW vw)

-- Try to find an empty VW belonging to the given group _in the current screen_,
-- and return its name. If group is full, then return the current VW.
tryVWofGroup :: MyVWGroup -> X VirtualWorkspace
tryVWofGroup g = do
	vw0 <- findWorkspace getSortByIndex Next (emptyVWinGrp g) 0
	vw1 <- findWorkspace getSortByIndex Next (emptyVWinGrp g) 1
	-- If current VW also happens to be an empty VW of the group g, then return
	-- this VW.
	return $ if isVWinGroups (removePWtag vw0) [g]
		then vw0
		else vw1

spawnIfGrpNotFull :: MyVWGroup -> String -> X ()
spawnIfGrpNotFull g command = do
	vw <- tryVWofGroup g
	let
		vw' = removePWtag vw
	when (isVWinGroups vw' [g])
		$ spawn command

spawnIfGrpTopVWNotFull :: MyVWGroup -> String -> X ()
spawnIfGrpTopVWNotFull g command = do
	vw <- tryVWofGroup g
	let
		vw' = removePWtag vw
	when (isVWinGroups vw' [g] && vw' == head (getVWsOfGroup $ getGroupOfVW vw'))
		$ spawn command

myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
	-- modm-button1 (left-click): Set the window to floating mode and move by
	-- dragging.
	[ ( (modm, button1)
	, \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
	)
	-- modm-button2 (middle-click): Raise the window to the top of the stack.
	-- This binding is almost never used, but we keep it here for instructive
	-- purposes.
	, ((modm, button2), \w -> focus w >> windows W.shiftMaster)
	,
	-- modm-button3 (right-click): Set the window to floating mode and resize by
	-- dragging.
	( (modm, button3)
	, \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
	)
	-- You may also bind events to the mouse scroll wheel (button4 and button5).
	]

-- Tall is the default tiling algorithm, which partitions the screen into two
-- panes. It takes three arguments x y z, where x is the default number of
-- windows in the master pane, y is the percent of the screen to increment by
-- when resizing panes, and z is the default proportion of the screen occupied
-- by the master pane.
defaultLayout :: Choose (Mirror Tall) (Choose Tall (XLL.ModifiedLayout WithBorder Full)) Window
defaultLayout = (Mirror $ tiled 1) ||| tiled 1 ||| noBorders Full
	where
	tiled nmaster = Tall nmaster delta ratio
	delta = 3/100
	ratio = 1/2

layoutNoMirror :: Choose ResizableTall (XLL.ModifiedLayout WithBorder Full) Window
layoutNoMirror = ResizableTall 0 (3/100) (1/2) [] ||| noBorders Full

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
myManageHook :: ManageHook
myManageHook = composeOne $
	[ className =? "MPlayer"                    -?> doFloat
	, className =? "Gimp"                       -?> doFloat
	, className =? "Agave"                      -?> doCenterFloat
	, resource  =? "desktop_window"             -?> doIgnore
	, resource  =? "kdesktop"                   -?> doIgnore
	, resource  =? "vlc"                        -?> doCenterFloat
	, resource  =? "WeeChat"                    -?> doShift "0"
	, title     =? "rtorrent"                   -?> doShift "0"
	, title     =? "htop"                       -?> doShift "0"
	, stringProperty "WM_ICON_NAME" =? "iftop"  -?> doShift "0"
	, title     =? "alsamixer"                  -?> doShift "0"
	, resource  =? "floatme"                    -?> doCenterFloat
	, className =? "qutebrowser"                -?> doShift =<< liftX (tryVWofGroup Net)
	, resource  =? "Navigator"                  -?> doShift =<< liftX (tryVWofGroup Net)
	, className =? "Evince"                     -?> doShift =<< liftX (tryVWofGroup Work)
	, resource  =? ".evince-wrapped"            -?> doShift =<< liftX (tryVWofGroup Work)
	, className =? "Blender:Render"             -?> doFloat
	, resource  =? "Browser"                    -?> doFloat
	, className =? "Galculator"                 -?> doCenterFloat
	, className =? "Gcalctool"                  -?> doCenterFloat
	, className =? "XClock"                     -?> doCenterFloat
	, className =? "Audacity"                   -?> doFloat
	, className =? "Gitk"                       -?> doCenterFloat
	, className =? "XDvi"                       -?> doCenterFloat
	, className =? "Scid"                       -?> doFloat
	, title     =? "Scid"                       -?> doFloat
	, className =? "Toplevel"                   -?> doFloat -- Scid's many popup windows
	, className =? "Pychess"                    -?> doFloat
	, className =? "Glchess"                    -?> doFloat
	, className =? "Raptor"                     -?> doFloat
	, className =? "Smplayer"                   -?> doFloat
	, className =? "linux_client"               -?> doFloat
	, className =? "Bsnes"                      -?> doCenterFloat
	, className =? "Phoenix"                    -?> doCenterFloat
	, className =? "VirtualBox"                 -?> doFloat
	, className =? "libreoffice-writer"         -?> doFloat
	, className =? "Xsane"                      -?> doFloat
	, className =? "Spektra"                    -?> doFloat
	, className =? "Glade"                      -?> doFloat
	, className =? "Anki"                       -?> doFloat
	, className =? "Qcp"                        -?> doFloat
	, className =? "mupen64plus"                -?> doFloat
	]
	++
	-- This is useful for auto-moving a terminal screen we spawn elsewhere in
	-- this config file to a particular workspace.
	[ resource =? ("atWorkspace" ++ s) -?> doShift s
	| s <- map show ([0..9]::[Int]) ++ map (('F':) . show) ([1..10]::[Int])
	]
	++
	-- Force new windows down (i.e., if a screen has 1 window (master) and we
	-- spawn a new window, don't become the new master window). See "Make new
	-- windows appear 'below' rather than 'above' the current window" at
	-- https://wiki.haskell.org/Xmonad/Frequently_asked_questions.
	[ return True -?> doF W.swapDown
	]

myStartupHook :: String -> X ()
myStartupHook hostname = do
	spawnIfGrpTopVWNotFull Net "qutebrowser"
	spawnIfGrpTopVWNotFull Work $ term1 ++ " -name atWorkspace1"
	spawnIfGrpTopVWNotFull Sys $ term1 ++ " -e htop"
	when (isUbuntu hostname) spawnWorkStuff
	when (isPersonal hostname) systemUtils
	where
	systemUtils = spawnIfGrpNotFull Sys $ term2 ++ " -e rtorrent"
	spawnWorkStuff = do
		spawnIfGrpNotFull Work "emacs /home/larver/k/notes_work_imvu/wlog.org"
		spawnIfGrpNotFull Work $ term2 ++ " -name floatme -e ssh-add"

main :: IO ()
main = do
	nScreens <- countScreens
	hostname <- fmap nodeName getSystemID
	if isPortraitMonitorLayout hostname
		then xmonad (myconf hostname nScreens) {layoutHook = layoutNoMirror}
		else xmonad $ myconf hostname nScreens
	where
	myconf hostname nScreens = def
		{ terminal           = "urxvt"
		, focusFollowsMouse  = True
		, clickJustFocuses   = True
		, borderWidth        = 1
		-- Use 'mod3' from 'xmodmap' output as our 'modMask' key. We alias it
		-- (XMonad.modMask) it as `modm` in our configuration above. Either
		-- before or immediately after XMonad starts, 'mod3' should be populated
		-- with some special key. This is handled usually by xsession scripts.
		-- On Arch Linux, this is `~/.xinitrc` because we use `startx` there. On
		-- NixOS, it is baked in directly to the system configuration file under
		-- the `services.xserver.displayManager.sessionCommands` option.
		, modMask            = mod3Mask
		, workspaces         = withScreens nScreens $ map fst myWorkspaceGroups
		, normalBorderColor  = "#000000"
		, focusedBorderColor = "#ffffff"
		, keys               = myKeys hostname
		, mouseBindings      = myMouseBindings
		, layoutHook         = defaultLayout
		, manageHook         = myManageHook
		, handleEventHook    = mempty
		, logHook            = mempty
		, startupHook        = myStartupHook hostname
		}
