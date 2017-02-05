module Main where

import Control.Monad
import Data.Array.IO
import Data.Maybe
	( isJust
	, isNothing
	)
import System.Exit
import System.Posix.Unistd
	( nodeName
	, getSystemID
	)
import System.Random
import XMonad
import XMonad.Actions.CycleWS
	( Direction1D
		( Next
		, Prev)
	, WSType (WSIs)
	, findWorkspace
	, moveTo
	, screenBy
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
	( doCenterFloat )
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

data MyWSGroup
	= Work
	| Net
	| Misc
	| Sys
	deriving (Eq, Ord, Enum, Show)

-- Use an association list for more flexible custom workspace manipulation.
--
-- The fact that we do not necessarily follow the ordering of the MYWSGroup data
-- type allows for maximum flexibility: we can either directly choose, via xK_1
-- ... xK_F12 keys what we want (like we did in the past), but at the same time
-- we can also choose to navigate via the ordering in MyWSGroup via the custom
-- diffGrpWS function.
myWorkspaceGroups :: [(WorkspaceId, MyWSGroup)]
myWorkspaceGroups =
	[ ("1",   Work)
	, ("2",   Work)
	, ("3",   Work)
	, ("4",   Work)
	, ("5",   Work)
	, ("6",   Work)
	, ("7",   Net)
	, ("8",   Net)
	, ("9",   Net)
	, ("0",   Sys)
	, ("F1",  Misc)
	, ("F2",  Misc)
	, ("F3",  Misc)
	, ("F4",  Misc)
	, ("F5",  Misc)
	, ("F6",  Misc)
	, ("F7",  Misc)
	, ("F8",  Misc)
	, ("F9",  Misc)
	, ("F10", Misc)
	]

myWorkspaces :: [WorkspaceId]
myWorkspaces = map fst myWorkspaceGroups

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

	-- Go to any non-empty WS, except those WS belonging to the given WS Groups
	-- (for making sure that our "desk" is clean before we log off/shutdown).
	, ((modm,   xK_n            ), moveTo Next $ nonEmptyWSExceptGrps [Sys])

	-- Go to WS displayed previously.
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
	[((modm .|. mask, k), windows $ f i)
		| (i, k) <- zip (XMonad.workspaces conf) ([xK_1..xK_9] ++ [xK_0] ++ [xK_F1..xK_F10])
		, (f, mask) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
	++
	-- modm-{h,l}: Switch to prev/next Xinerama screens.
	-- modS-{h,l}: Move client to prev/next screen.
	[((m .|. modm, key), (screenWorkspace =<< sc) >>= flip whenJust (windows . f))
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
	supr = mod4Mask
	suprS = supr .|. shiftMask
	relativeDimenions
		= W.RationalRect marginLeft marginTop windowWidth windowHeight
		where
		marginLeft = 1/6
		marginTop = 1/6
		windowWidth = 2/3
		windowHeight = 2/3

-- An empty WS belonging to the same group as the current one.
emptyGrpWS :: WSType
emptyGrpWS = WSIs $ do
	currentWS <- gets (W.currentTag . windowset)
	let
		currentWSGrp = getWSGroup currentWS
		isEmpty = isNothing . W.stack
		isCurrentWSGrp = isWSGroup currentWSGrp . W.tag
	return (\w -> isEmpty w && isCurrentWSGrp w)

-- An empty WS belonging to the given group.
emptyWSGrp :: MyWSGroup -> WSType
emptyWSGrp grp = WSIs $ do
	let
		isEmpty = isNothing . W.stack
		isMemberOfGivenGrp = isWSGroup grp . W.tag
	return (\w -> isEmpty w && isMemberOfGivenGrp w)

-- A non-empty WS belonging to any group except those in the given group list.
nonEmptyWSExceptGrps :: [MyWSGroup] -> WSType
nonEmptyWSExceptGrps grps = WSIs $ do
	let
		isNonEmpty = isJust . W.stack
		isMemberOfGivenGrps = isWSGroups grps . W.tag
	return (\w -> isNonEmpty w && not (isMemberOfGivenGrps w))

getWSids :: MyWSGroup -> [WorkspaceId]
getWSids g
	= map fst
	$ filter ((== g) . snd) myWorkspaceGroups

getWSGroup :: WorkspaceId -> MyWSGroup
getWSGroup wsid
	= head
	. map snd
	$ filter ((== wsid) . fst) myWorkspaceGroups

-- Confirms if a given WorkspaceId str is part of a given MyWSGroup g.
--
-- The order of the arguments is "backwards" because we compose this function
-- with StackSet's tag function, which returns the WorkspaceId, e.g., "(\w ->
-- isWSGroup Net . W.tag $ w)".
isWSGroup :: MyWSGroup -> WorkspaceId -> Bool
isWSGroup grp str
	| lookup str myWorkspaceGroups == Just grp = True
	| otherwise = False

-- Same as isWSGroup, but takes a list of valid MyWSGroups instead of just a
-- single MyWSGroup.
isWSGroups :: [MyWSGroup] -> WorkspaceId -> Bool
isWSGroups grps str
	| isJust (lookup str myWorkspaceGroups') = True
	| otherwise = False
	where
	myWorkspaceGroups' = filter (\(_, y) -> elem y grps) myWorkspaceGroups

-- Try to find an empty WSID belonging to the given group, and return its name.
-- If group is full, then return the current WSID.
tryGetEmptyWSIDofGroup :: MyWSGroup -> X WorkspaceId
tryGetEmptyWSIDofGroup g = do
	-- ws0: An empty WS of group g 0 WSs away from current WS.
	ws0 <- findWorkspace getSortByIndex Next (emptyWSGrp g) 0
	-- ws1: An empty WS of group g 1 WSs away from current WS.
	ws1 <- findWorkspace getSortByIndex Next (emptyWSGrp g) 1
	-- If current WS also happens to be an empty WS of the group g, then return
	-- this WSID; the empty WS of group g, NOT counting the current WS; if such
	-- a WS does not exist, then ws1 is just the current WS.
	return $ if isWSGroup g ws0
		then ws0
		else ws1

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
myManageHook = composeAll $
	[ className =? "MPlayer"                    --> doFloat
	, className =? "Gimp"                       --> doFloat
	, className =? "Agave"                      --> doCenterFloat
	, resource  =? "desktop_window"             --> doIgnore
	, resource  =? "kdesktop"                   --> doIgnore
	, resource  =? "vlc"                        --> doCenterFloat
	, resource  =? "WeeChat"                    --> doShift "0"
	, title     =? "rtorrent"                   --> doShift "0"
	, title     =? "htop"                       --> doShift "0"
	, stringProperty "WM_ICON_NAME" =? "iftop"  --> doShift "0"
	, title     =? "alsamixer"                  --> doShift "0"
	, resource  =? "floatme"                    --> doCenterFloat
	, resource  =? "Navigator"                  --> doShift =<< liftX (tryGetEmptyWSIDofGroup Net)
	, className =? "Evince"                     --> doShift =<< liftX (tryGetEmptyWSIDofGroup Misc)
	, resource  =? ".evince-wrapped"            --> doShift =<< liftX (tryGetEmptyWSIDofGroup Misc)
	, className =? "Blender:Render"             --> doFloat
	, resource  =? "Browser"                    --> doFloat
	, className =? "Galculator"                 --> doCenterFloat
	, className =? "Gcalctool"                  --> doCenterFloat
	, className =? "XClock"                     --> doCenterFloat
	, className =? "Audacity"                   --> doFloat
	, className =? "Gitk"                       --> doCenterFloat
	, className =? "XDvi"                       --> doCenterFloat
	, className =? "Scid"                       --> doFloat
	, title     =? "Scid"                       --> doFloat
	, className =? "Toplevel"                   --> doFloat -- Scid's many popup windows
	, className =? "Pychess"                    --> doFloat
	, className =? "Glchess"                    --> doFloat
	, className =? "Raptor"                     --> doFloat
	, className =? "Smplayer"                   --> doFloat
	, className =? "linux_client"               --> doFloat
	, className =? "Bsnes"                      --> doCenterFloat
	, className =? "Phoenix"                    --> doCenterFloat
	, className =? "VirtualBox"                 --> doFloat
	, className =? "libreoffice-writer"         --> doFloat
	, className =? "Xsane"                      --> doFloat
	, className =? "Spektra"                    --> doFloat
	, className =? "Glade"                      --> doFloat
	, className =? "Anki"                       --> doFloat
	, className =? "Qcp"                        --> doFloat
	, className =? "mupen64plus"                --> doFloat
	]
	++
	-- This is useful for auto-moving a terminal screen we spawn elsewhere in
	-- this config file to a particular workspace.
	[ resource =? ("atWorkspace" ++ s) --> doShift s
	| s <- map show ([0..9]::[Int]) ++ map (('F':) . show) ([1..10]::[Int])
	]

myStartupHook :: String -> X ()
myStartupHook hostname = do
	spawnIfGrpTopWSNotFull Net "qutebrowser"
	spawnIfGrpTopWSNotFull Work $ term1 ++ " -name atWorkspace1"
	spawnIfGrpTopWSNotFull Sys $ term1 ++ " -e htop"
	when (isUbuntu hostname) spawnWorkStuff
	when (isPersonal hostname) systemUtils
	where
	systemUtils = spawnIfGrpNotFull Sys $ term2 ++ " -e rtorrent"
	spawnWorkStuff = do
		spawnIfGrpNotFull Work "emacs /home/larver/k/notes_work_imvu/wlog.org"
		spawnIfGrpNotFull Work $ term2 ++ " -name floatme -e ssh-add"

spawnIfGrpNotFull :: MyWSGroup -> String -> X ()
spawnIfGrpNotFull g command = do
	wsid <- tryGetEmptyWSIDofGroup g
	when (isWSGroup g wsid)
		$ spawn command

spawnIfGrpTopWSNotFull :: MyWSGroup -> String -> X ()
spawnIfGrpTopWSNotFull g command = do
	wsid <- tryGetEmptyWSIDofGroup g
	when (isWSGroup g wsid && wsid == head (getWSids . getWSGroup $ wsid))
		$ spawn command

main :: IO ()
main = do
	hostname <- fmap nodeName getSystemID
	if isPortraitMonitorLayout hostname
		then xmonad (myconf hostname) {layoutHook = layoutNoMirror}
		else xmonad $ myconf hostname
	where
	myconf hostname = def
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
		, workspaces         = myWorkspaces
		, normalBorderColor  = "#000000"
		, focusedBorderColor = "#ffffff"
		, keys               = myKeys hostname
		, mouseBindings      = myMouseBindings
		, layoutHook         = defaultLayout
		, manageHook         = myManageHook
		, handleEventHook    = mempty
		, logHook            = return ()
		, startupHook        = myStartupHook hostname
		}
