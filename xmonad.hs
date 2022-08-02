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

import XMonad.Util.WorkspaceCompare as WC ( getSortByIndex )
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

data Direction3D = XU -- X++
		 | XD -- X--
		 | YU -- Y--
		 | YD -- Y++
		 | ZU -- Z++
		 | ZD -- Z--
		   deriving (Eq, Read, Show, Ord, Enum, Bounded)
--		   deriving (Eq, Read, Show, Ord, Enum, Bounded, Typeable)

--numSubWS = 3
--numWS = 9

workspaces :: [WorkspaceId]
workspaces = map show [1 .. 9 :: Int] ++ ["0"]
--workspaces = ["web", "lshell", "rshell", "chat" ] ++ map show [5..(numSubWS * numWS)]

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
	xmonad $ defaultConfig {
	  terminal = "lxterminal"
--	  terminal = "gnome-terminal --hide-menubar"
--	, modMask = mod4Mask
	, XMonad.workspaces = Main.workspaces
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
	, (modm .|. shiftMask, xK_c)
	, (modm .|. shiftMask, xK_j)
	, (modm .|. shiftMask, xK_k)
	, (modm, xK_z)
	, (modm, xK_m)
	]
--	++ removeWorkspaceSwitches
--	where removeWorkspaceSwitches =
--		zip (repeat modm) [ xK_1 .. xK_9 ]
--		++ zip (repeat (shiftMask .|. modm)) [ xK_1 .. xK_9 ]

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

layout = Full ||| tiled ||| Mirror tiled
	where tiled	= Tall nmaster delta ratio
	      nmaster	= 1
	      ratio	= 1/2
	      delta	= 3/100

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
--	, ((modm .|. shiftMask, xK_t), swapUp)
	, ((modm .|. shiftMask, xK_t), spawnOn "rshell" "gnome-terminal --hide-menubar")
--	, ((modm .|. shiftMask, xK_r), windows W.swapDown)
--	, ((modm .|. shiftMask, xK_t), windows W.swapUp)
	, ((modm .|. shiftMask, xK_c), kill)
--	, ((modm .|. shiftMask, xK_c), killAll)
	, ((modm, xK_f), fullFloatFocused)
	, ((modm, xF86XK_Launch1), switchKeyboardVariant)
	, ((modm, xK_slash), switchKeyboardVariant)
--	, ((modm, xK_m), spawnOn "rshell" "lxterminal")
--	, ((modm, xK_m), bindOn $ zip (XMonad.workspaces conf) (replicate 9 $ spawnOn "rshell" "lxterminal"))
	, ((modm, xK_m), bindOn [("0", sendKey modm xK_m), ("", spawnOn "rshell" "lxterminal")])
--	, ((modm, xK_m), bindOn [("0", return ()), ("", spawnOn "rshell" "lxterminal")])
	, ((modm, xK_z), return ())
	, ((modm, xK_b), sendKey modm xK_b)
	, ((modm,               xK_Up   ), focusGroupUp)
	, ((modm .|. mod5Mask,  xK_l    ), focusGroupUp)
	, ((modm,               xK_Down ), focusGroupDown)
	, ((modm .|. mod5Mask,  xK_a    ), focusGroupDown)
	, ((modm .|. shiftMask, xK_Up   ), moveToGroupUp False)
	, ((modm .|. mod5Mask .|. shiftMask, xK_l ), moveToGroupUp False)
	, ((modm .|. shiftMask, xK_Down ), moveToGroupDown False)
	, ((modm .|. mod5Mask .|. shiftMask, xK_a ), moveToGroupDown False)
--	, ((modm, xK_Escape), spawnOn "rshell" "lxterminal")
--	, ((modm, xK_Escape), CW.toggleOrView "0")
	, ((modm, xK_Escape), escCMD)
	, ((modm, xK_asciicircum), escCMD)
	, ((modm .|. shiftMask, xK_Escape), toggleOrShift "0")
--	, ((modm .|. shiftMask, xK_Escape), shiftWithZero)
--	, ((modm), submap . M.fromList $ [ ((modm), spawn "xmessage 'yes!'") ]
--	, ((modm, xK_Page_Up), workspaceUp )
	]
	++
	-- mod-[1..9] %! Switch to workspace N
	-- mod-shift-[1..9] %! Move client to workspace N
	[((m .|. modm .|. mod5Mask, k), windows $ f i)
	| (i, k) <- zip (XMonad.workspaces conf) keys
	, (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
	where keys =	[ xK_m, xK_ssharp, xK_j	-- 1, 2, 3
			, xK_n, xK_r, xK_t	-- 4, 5, 6
			, xK_h, xK_g, xK_f]	-- 7, 8, 9

{-	++
	-- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
	-- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
	[((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
		| (key, sc) <- zip [xK_v, xK_l, xK_c] [0..]
		, (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]	
	++ addNewWorkspaceSwitches
	++ addSubWorkspaceSwitches
	where	addNewWorkspaceSwitches =
			[((m .|. modm, k), windows $ f i)
			| (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_5] ++ [ xK_F1 .. xK_F4 ])
			, (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
		addSubWorkspaceSwitches =
			[ ((modm, xK_Page_Up), nextSubWSZ)
			, ((modm, xK_Page_Down), prevSubWSZ) ]
-}
nextSubWSZ :: X ()
nextSubWSZ = switchWorkspace 9

prevSubWSZ :: X ()
prevSubWSZ = switchWorkspace (-9)

switchWorkspace :: Int -> X ()
switchWorkspace d = wsBy d >>= windows . W.greedyView

wsBy :: Int -> X (WorkspaceId)
wsBy = CW.findWorkspace WC.getSortByIndex CW.Next CW.AnyWS

{--findSubWorkspace :: X Int
findSubWorkspace = do
	ws <- gets windowset
	let	cur	= W.workspace (W.current ws)
		mCurIx	= findWsIndex cur (S.workspaces ws)
	return mCurIx --}

{-- findWsIndex :: WindowSpace -> [WindowSpace] -> Int
findWsIndex ws wss = M.findIndex ((== W.tag ws) . W.tag) wss --}

pageUpDownWSSwitch :: XConfig t -> [((KeyMask, KeySym), X ())]
pageUpDownWSSwitch conf@(XConfig {XMonad.modMask = modm}) = []

-- myManageHook = composeOne [ isFullscreen -?> doFullFLoat ]
fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery 
                   doCenterFloat f

-- Autostart
-- startup :: X ()
startup = do
--	sp <- mkSpawner
--	spawn "setxkbmap lv && xmodmap $HOME/neon_de.xmodmap && xset -r 51"
	spawnOnce "pulseaudio -D"
	spawnOnce "setxkbmap lv && xmodmap $HOME/neon-def_de.xmodmap"
	setWMName "LG3D" -- Breaks current gtk3 (this was a workaround for JDK6)
	spawnOnce "$HOME/.xmonad/autostart.sh"
	spawnOn "lshell" "lxterminal"
	spawnOn "0" "lxterminal"
	spawnOn "2" "lxterminal -e 'bluetoothctl'"
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
