{-# OPTIONS_GHC -Wno-deprecations #-}

-- merged from http://haskell.org/haskellwiki/Xmonad/Config_archive/sereven's_xmonad.hs_one-host
-- and http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Doc-Extending.html
import XMonad
-- import qualified XMonad.Core
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Layout.ResizableTile
import XMonad.Actions.SpawnOn
import XMonad.Util.SpawnOnce (spawnOnce)
import Data.Monoid
import XMonad.Layout.NoBorders (smartBorders)

import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Util.CustomKeys
import Graphics.X11.ExtraTypes.XF86

import XMonad.Actions.CycleWS as CW ( findWorkspace, Direction1D(Next), WSType(AnyWS), toggleOrView, shiftToPrev, toggleOrDoSkip )
import XMonad.Actions.PerWorkspaceKeys (bindOn)

import XMonad.Util.Types
import XMonad.Util.Timer
import XMonad.Util.Paste (sendKey)

import Graphics.X11.Xlib.Extras as EV

import XMonad.Hooks.ServerMode as SM
import XMonad.Actions.Commands as AC

-- import XMonad.Core as XMonad hiding
--    (workspaces,manageHook,numlockMask,keys,logHook,startupHook,borderWidth,mouseBindings
--    ,layoutHook,modMask,terminal,normalBorderColor,focusedBorderColor,focusFollowsMouse
--    ,handleEventHook)
import qualified XMonad.Core as XMonad
--    (workspaces,manageHook,numlockMask,keys,logHook,startupHook,borderWidth,mouseBindings
--    ,layoutHook,modMask,terminal,normalBorderColor,focusedBorderColor,focusFollowsMouse
--    ,handleEventHook)

import qualified XMonad.StackSet as S hiding (filter)
--import Data.Typeable (Typeable)

import XMonad.Layout.Groups.Helpers
import qualified XMonad.Layout.Groups as G

-- Support wmctrl and (partially) OBS Xcomposite window capture
import XMonad.Hooks.EwmhDesktops

-- Layouts modifiers
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.NoBorders
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

-- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.OneBig
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import XMonad.Layout.IM (withIM, Property(Role))

-- https://dev.to/l04db4l4nc3r/xmonad-hackability-at-a-cost-390a
-- https://github.com/L04DB4L4NC3R/DEC/blob/master/.xmonad/xmonad.hs


data Direction3D = XU -- X++
		 | XD -- X--
		 | YU -- Y--
		 | YD -- Y++
		 | ZU -- Z++
		 | ZD -- Z--
		   deriving (Eq, Read, Show, Ord, Enum, Bounded)
--		   deriving (Eq, Read, Show, Ord, Enum, Bounded, Typeable)

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 9 :: Int] ++ ["0"]

myTerminalCmd = "xfce4-terminal --disable-server --hide-menubar"

switchKeyboardVariant = spawn "if setxkbmap -query | grep -q '^layout:.*us'; then setxkbmap lv && xmodmap $HOME/neon-def_de.xmodmap; else setxkbmap us; fi"

myManageHook :: ManageHook
myManageHook = composeAll
	[ className =? "Vncviewer"	--> doShift "0"
	, manageDocks
	]

myServedCommands = return
	[ ("push-hidden", shiftWithZero)
	, ("toggle-hidden", escCMD)
--	, ("toggle-hidden", CW.toggleOrView "0")
--	, ("shift-to-hidden", shiftToZero)
--	, ("shift-from-hidden", shiftFromZero)
	, ("blau", spawn "xmessage 'der Himmel war BLAU'")
	, ("rot", spawn "xmessage 'der Himmel war ROT'")
	]

main = do
	xmonad $ ewmh defaultConfig {
	terminal = myTerminalCmd
--	  terminal = "lxterminal"
--	  terminal = "gnome-terminal --hide-menubar"
	--, modMask = mod4Mask
	, modMask = mod1Mask
	, workspaces = myWorkspaces
--	, XMonad.layoutHook = layout
--	, XMonad.borderWidth = 1
	, XMonad.focusedBorderColor = "blue"
	, keys = customKeys toRemove toAdd
--	, manageHook = manageSpawn <+> manageHook defaultConfig
	, manageHook = manageSpawn <+> myManageHook <+> manageHook defaultConfig
	, startupHook = startup
	, layoutHook = G.group (smartBorders layout) Full
--	, layoutHook = G.group (smartBorders (layoutHook defaultConfig)) Full
	, focusFollowsMouse  = False
	, handleEventHook = serverModeEventHook' myServedCommands
	} 

-- All available keys: https://github.com/xmonad/X11/blob/master/Graphics/X11/Types.hsc

-- Key bindings being removed from the default config
toRemove conf@(XConfig {XMonad.modMask = modm}) =
	[ (modm, xK_h)
	, (modm, xK_j)
	, (modm, xK_k)
	, (modm, xK_l)
	, (modm, xK_t)
	, (modm, xK_w)
	, (modm, xK_e)
	, (modm, xK_r)
	, (modm, xK_v)
	, (modm, xK_l)
	, (modm, xK_c)
	, (modm, xK_z)
	, (modm, xK_m)
	, (modm .|. shiftMask, xK_h)
	, (modm .|. shiftMask, xK_j)
	, (modm .|. shiftMask, xK_k)
	, (modm .|. shiftMask, xK_l)
	, (modm .|. shiftMask, xK_t)
	, (modm .|. shiftMask, xK_w)
	, (modm .|. shiftMask, xK_e)
	, (modm .|. shiftMask, xK_r)
	, (modm .|. shiftMask, xK_v)
	, (modm .|. shiftMask, xK_l)
	, (modm .|. shiftMask, xK_c)
	, (modm .|. shiftMask, xK_z)
	, (modm .|. shiftMask, xK_m)
	]

-- Found out about xF86XK_Launch1 via 'xev' (keycode 156)
-- via 'xmodmap -pke' (keycode 156 -> XF86Launch1)

killAll = withWindowSet $ mapM_ killWindow . W.index

-- killAll >> (setLayout $ XMOnad.layoutHook conf))

escCMD = do
--	bindOn [ ("0", return ()), ("", spawn "xdotool keyup Super_L; xmodmap -e 'clear mod1' -e 'clear mod4' -e 'keycode 64 = F24' -e 'add mod4 = F24'")]
--	bindOn	[ ("0", spawn "xdotool keyup F24; xmodmap -e 'clear mod1' -e 'clear mod4' -e 'keycode 64 = Super_L' -e 'add mod1 = Super_L Meta_L'")
--		, ("", spawn "xdotool keyup Super_L; xmodmap -e 'clear mod1' -e 'clear mod4' -e 'keycode 64 = F24' -e 'add mod4 = F24'")]
	bindOn	[ ("0", spawn "xdotool keyup Super_R; xmodmap -e 'clear mod1' -e 'clear mod4' -e 'keycode 64 = Super_L' -e 'add mod1 = Super_L Meta_L'")
		, ("", spawn "xdotool keyup Super_L; xmodmap -e 'clear mod1' -e 'clear mod4' -e 'keycode 64 = Super_R' -e 'add mod4 = Super_R'")]
	CW.toggleOrView "0"

shiftFromZero = do
--	spawn "xmessage hello from 0"
	windows $ W.shift "1"
--	CW.shiftToPrev
--	spawn 

shiftToZero = do
--	spawn "xmessage hello not from 0"
	windows $ W.shift "0"

toggleOrShift :: WorkspaceId -> X ()
toggleOrShift = CW.toggleOrDoSkip [] W.shift

-- If hidden workspace 0 is selected, move window to previous WS,
-- else move current window to hidden workspace 0.
shiftWithZero = do
	bindOn [ ("0",  shiftFromZero), ("", shiftToZero)]

layout = Full ||| tiled ||| Mirror tiled ||| Grid (16/10) ||| myGrid ||| myOneBig ||| OneBig (5/9) (8/12)
	where tiled	= Tall nmaster delta ratio
	      nmaster	= 1
	      ratio	= 1/2
	      delta	= 3/100
	      myGrid	= limitWindows 12 $ spacing 6 $ mkToggle (single MIRROR) $ Grid (16/10)
	      myOneBig	= limitWindows 6  $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (5/9) (8/12)

-- mod-[1..9] %! Switch to workspace N
-- mod-shift-[1..9] %! Move client to workspace N
myWorkspaceKeyActions modm conf = do
	(idx, key) <- zip (XMonad.workspaces conf) keys
	(func, mkey) <- [(W.greedyView, 0), (W.shift, shiftMask)]
	return ((mkey .|. modm .|. mod3Mask, key), windows $ func idx)
--	return ((mkey .|. modm .|. mod5Mask, key), windows $ func idx)
	where keys =	[ xK_m, xK_ssharp, xK_j	-- 1, 2, 3
			, xK_n, xK_r, xK_t	-- 4, 5, 6
			, xK_h, xK_g, xK_f]	-- 7, 8, 9

-- mod-{v,l,c} %! Switch to physical/Xinerama screens 1, 2, or 3
-- mod-shift-{v,l,c} %! Move client to screen 1, 2, or 3
myXineramaKeyActions modm conf = do
	(key, scr) <- zip keys [0..]
	(func, mkey) <- [(W.view, 0), (W.shift, shiftMask)]
	return ((mkey .|. modm, key), screenWorkspace scr >>= flip whenJust (windows . func))
	where keys = [xK_v, xK_l, xK_c]

-- Key bindings being added
toAdd conf@(XConfig {XMonad.modMask = modm}) =
	[ ((modm, xK_n), sendMessage Shrink)
	, ((modm, xK_d), sendMessage Expand)
--	, ((modm, xK_r), windows W.focusDown)
	, ((modm, xK_r), focusDown)
--	, ((modm, xK_t), windows W.focusUp)
	, ((modm, xK_t), focusUp)
	, ((modm, xK_w), withFocused $ windows . W.sink)
	, ((modm .|. shiftMask, xK_r), swapDown)
	, ((modm .|. shiftMask, xK_t), swapUp)
--	, ((modm .|. shiftMask, xK_r), windows W.swapDown)
--	, ((modm .|. shiftMask, xK_t), windows W.swapUp)
	, ((modm .|. shiftMask, xK_c), kill)
--	, ((modm .|. shiftMask, xK_c), killAll)
	, ((modm, xK_f), fullFloatFocused)
	, ((modm, xF86XK_Launch1), switchKeyboardVariant)
	, ((modm, xK_slash), switchKeyboardVariant)
--	, ((modm, xK_m), spawnOn "rshell" "lxterminal")
--	, ((modm, xK_m), bindOn $ zip (XMonad.workspaces conf) (replicate 9 $ spawnOn "rshell" "lxterminal"))
	, ((modm, xK_m), bindOn [("0", sendKey modm xK_m), ("", spawnOn "rshell" myTerminalCmd)])
--	, ((modm, xK_m), bindOn [("0", return ()), ("", spawnOn "rshell" "lxterminal")])
	, ((modm, xK_z), return ())
	, ((modm, xK_b), sendKey modm xK_b)
	, ((modm,               xK_Up   ), focusGroupUp)
	, ((modm .|. mod3Mask,  xK_l    ), focusGroupUp)
	, ((modm,               xK_Down ), focusGroupDown)
	, ((modm .|. mod3Mask,  xK_a    ), focusGroupDown)
	, ((modm .|. shiftMask, xK_Up   ), moveToGroupUp False)
	, ((modm .|. mod3Mask .|. shiftMask, xK_l ), moveToGroupUp False)
	, ((modm .|. shiftMask, xK_Down ), moveToGroupDown False)
	, ((modm .|. mod3Mask .|. shiftMask, xK_a ), moveToGroupDown False)
--	, ((modm, xK_Escape), spawnOn "rshell" "lxterminal")
--	, ((modm, xK_Escape), CW.toggleOrView "0")
	, ((modm, xK_Escape), escCMD)
	, ((modm, xK_asciicircum), escCMD)
	, ((modm .|. shiftMask, xK_Escape), toggleOrShift "0")
	, ((noModMask, xF86XK_MonBrightnessUp), spawn "brightnessctl set +5%")
	, ((noModMask, xF86XK_MonBrightnessDown), spawn "brightnessctl set 5%-")
	, ((noModMask, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
	, ((noModMask, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
	, ((noModMask, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
	, ((noModMask, xF86XK_AudioMicMute), spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle")
--	, ((modm .|. shiftMask, xK_Escape), shiftWithZero)
--	, ((modm), submap . M.fromList $ [ ((modm), spawn "xmessage 'yes!'") ]
--	, ((modm, xK_Page_Up), workspaceUp )
	]
	++ myWorkspaceKeyActions modm conf ++ myXineramaKeyActions modm conf

-- myManageHook = composeOne [ isFullscreen -?> doFullFLoat ]
fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery 
                   doCenterFloat f

-- Autostart
-- startup :: X ()
startup = do
--	sp <- mkSpawner
--	spawn "setxkbmap lv && xmodmap $HOME/neon_de.xmodmap && xset -r 51"
--	spawnOnce "pulseaudio -D"
--	spawnOnce "systemctl --user restart pulseaudio.service"
	--spawnOnce "setxkbmap lv && xmodmap $HOME/neon-def_de.xmodmap"
	spawnOnce "setxkbmap custom"
	setWMName "LG3D" -- Breaks current gtk3 (this was a workaround for JDK6)
	spawnOn "5" myTerminalCmd
	spawnOn "1" (myTerminalCmd ++ " -e 'sh -c \"screen -ls 'xmonad-bg' || screen -S 'xmonad-bg'\"'")
	spawnOn "1" (myTerminalCmd ++ " -e 'sh -c \"pidof bluetoothctl || bluetoothctl\"'")
	spawn "$HOME/.xmonad/autostart.sh"
--	myTimer
--	spawnOn "web" "/usr/lib/iceweasel/firefox-bin"

--myTimer :: X ()
--myTimer = do
--	id <- startTimer 1
--	handleTimer id (ClientMessageEvent {ev_message_type = mt, ev_data = dt}) timerCallback
--	return
	
--timerCallback :: X (Maybe a)
--timerCallback = spawn "/home/linus/.xmonad/test.sh"

--	spawnOn sp "lshell" "/home/linus/.xmonad/autostart.sh"
--	spawnOn sp "lshell" "gnome-terminal --hide-menubar -x screen"
--	spawn "gnome-terminal --hide-menubar -x screen"
--	spawn "setxkbmap de neo -option"
--	spawn "setxkbmap -layout de,de -variant neo,basic -option -option grp:lwin_toggle"
--	spawn "xscreensaver"


-- http://xmonad.org/xmonad-docs/X11/Graphics-X11-Types.html
--xK_Page_Up
--xK_Page_Down
