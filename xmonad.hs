-- copyright 2008-2013 Elon Flegenheimer
-- license: as-is
--
-- xmonad --recompile
-- mod-q
-- mod-shift-space
--
--
-- TODO
-- finish icons for layouts
-- extract utility code
--

import Control.Monad
import Data.List
import Data.Ratio ((%))
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xinerama
import Graphics.X11.Xlib
import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import System.Directory
import System.Exit
import System.IO
import System.Posix.Unistd
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad hiding ((|||))
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook hiding (Never)
import XMonad.Layout.Accordion
import XMonad.Layout.ComboP
import XMonad.Layout.DwmStyle
import XMonad.Layout.Grid
import XMonad.Layout.IM as IM
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Magnifier
import XMonad.Layout.MosaicAlt
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.SimpleFloat
import XMonad.Layout.StackTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Prompt
import XMonad.Prompt.DirExec
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig(additionalKeysP, additionalKeys)
import XMonad.Util.Run

-- spawn in background (ensures exit 0)
spawn' x = spawn $ x ++ "&"

killPanels = do
    spawn' "killall conky dzen2"
    spawn' "cat /dev/null > /tmp/haskell.log"
    return ()

-- assumes main screen is 2nd screen and both monitors are 1920x1080
mainScreenX :: Int -> String
mainScreenX 1 = "0"
mainScreenX 2 = "1920"
 
getScreenCount' :: Num a => IO a
getScreenCount' = do
    d <- openDisplay ""
    screens  <- getScreenInfo d
    return $ fromIntegral $ length screens

conkyBar :: String -> String
conkyBar x = 
    intercalate " " 
        [ "conky -c ~/.xmonad/config/conky | dzen2 -x"
        , x
        , "-w 1920 -y 1064 -bg black -fg white -fn '-*-fixed-*-*-*-*-14-*-*-*-*-*-*-*'" ]

myStatusBarThemed = "-ta 'l' -bg '" ++ dustBackground ++ "' -h '" ++ barHeight ++ "' -fg '" ++ dustForeground ++ "' -fn '" ++ myFont ++ "'"
myStatusBar x = 
    intercalate " "
        [ "dzen2 -x"
        , show $ read x + 700
        , "-y '0' -w '600'"
        , myStatusBarThemed ]

debug' s = do
    spawn' $ "echo '" ++ (show s) ++ "' >> /tmp/haskell.log"

main = do
    screenCount <- getScreenCount'
    dzen <- spawnPipe $ myStatusBar $ mainScreenX screenCount
    -- debug' $ conkyBar $ mainScreenX screenCount
    spawn' $ conkyBar $ mainScreenX screenCount
    home <- getHomeDirectory
    hostname <- getHostName
    xmonad $ withUrgencyHook NoUrgencyHook $ ewmh gnomeConfig {
		terminal           = myTerminal
		, focusFollowsMouse  = True
		, borderWidth        = 1
		, modMask            = mod4Mask
		, workspaces         = myWorkspaces
		, normalBorderColor  = lightGray
		, focusedBorderColor = blue
		, keys               = myKeys
		, mouseBindings      = myMouseBindings
		, layoutHook         = myLayout
		, manageHook         = myManageHook
		, logHook			   = dynamicLogWithPP $ myDzenPP dzen home
		, startupHook        = myStartupHook
		} `additionalKeysP` (myAddKeys hostname)

myTerminal      = "gnome-terminal"

myWorkspaces    = map show [1..4] ++ 
                    [ "dev"
                    , "web"
                    , "media"
                    , "mail"
                    , "gimp"
                    , "im"
                    , "vm"
                    ]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
-- http://hackage.haskell.org/packages/archive/X11/1.5.0.0/doc/html/Graphics-X11-Types.html 
--
-- see 'xev' to capture key strokes
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

	(

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch a terminal (small font)
    , ((modm .|. shiftMask, xK_minus), spawn "gnome-terminal --profile=Small")

    -- launch browser
    , ((modm,               xK_f     ), spawn "firefox")

    -- launch gtd
    , ((modm,               xK_grave     ), unsafeSpawn "$HOME/bin/gtd")

    -- launch nautilus browser
    , ((modm,               xK_o     ), spawn "nautilus --no-desktop ~")
    , ((modm,               xK_d     ), spawn "nautilus --no-desktop ~/Desktop")

    -- close focused window 
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_F4    ), kill)

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
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- toggle the status bar gap (used with avoidStruts from Hooks.ManageDocks)
    , ((modm , xK_b ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), quitWithWarning)

    , ((controlMask, xK_q     		 ), spawn "notify-send 'consumed ctrl-q' -t 1000")

    -- Gnome's Print screen functionality seems borked when using xmonad
    , ((modm ,              xK_Print ), spawn "exe=`gnome-screenshot` && eval \"exec $exe\"")

    -- Switch workspaces (and move windows) horizontally
    -- , ((modm              , xK_Left  ), prevWS )
    , ((modm              , xK_Left  ), moveTo Prev NonEmptyWS )
    -- , ((modm              , xK_Right ), nextWS )
    , ((modm              , xK_Right ), moveTo Next NonEmptyWS )
    , ((modm .|. shiftMask, xK_Left  ), shiftToPrev )
    , ((modm .|. shiftMask, xK_Right ), shiftToNext )

    -- Restart xmonad
    , ((modm              , xK_q     ), killPanels >> restart "xmonad" True)

	-- Sleep machine; use sudoers to void pwd request
    , ((modm              , xK_End   ), spawn "sudo pm-suspend")

	-- dismount mt4g
    , ((modm .|. controlMask , xF86XK_Eject   ), spawn "umount /media/mt4g")

    -- Prompts
    --, ((modMask              , xK_a     ), dirExecPrompt myXPConfig spawn "/home/bla/.xmonad/scripts")
    , ((modm              , xK_i     ), shellPrompt myXPConfig)
    , ((modm              , xK_z     ), workspacePrompt myXPConfig (windows . W.view))
    , ((modm .|. shiftMask, xK_z     ), workspacePrompt myXPConfig (windows . W.shift))
    , ((modm              , xK_x     ), windowPromptGoto myXPConfig)
    , ((modm .|. shiftMask, xK_x     ), windowPromptBring myXPConfig)
    --, ((modm              , xK_m     ), manPrompt myXPConfig)

    -- Actions
    , ((modm , xK_BackSpace), toggleWS)

    -- Copy Window
    , ((modm              , xK_v     ), workspacePrompt myXPConfig (windows . copy))
    , ((modm .|. shiftMask, xK_v     ), killAllOtherCopies)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    -- next used greedy before
    [((m .|. modm, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    -- next used greedy before
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

	)

-- see /usr/include/X11/XF86keysym.h to include in normal section instead
mediaKeys =	[("<XF86AudioPlay>", spawn' "audacious -u"),
			 ("<XF86AudioStop>", spawn' "audacious -s"),
			 ("<XF86AudioNext>", spawn' "audacious -f"),
			 ("<XF86AudioPrev>", spawn' "audacious -r"),
			 ("<XF86Sleep>", spawn "sudo pm-suspend")
			]

screenKeyOrder10 = [(mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
                    | (key, scr)  <- zip "wer" [1,0]
                    , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]]

myAddKeys hostname = mediaKeys ++ case hostname of
    "w520" -> screenKeyOrder10
    _ -> []


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- Scrolls to the next workspace (mouse wheel up)
    , ((modMask, button4), (\w ->nextWS))

    -- Scrolls to the previous workspace (mouse wheel down)
    , ((modMask, button5), (\w ->prevWS))

    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
--myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full

mosaicLayout = MosaicAlt M.empty

full = noBorders Full

imLayout = named "IM" $
    combineTwoP (TwoPane 0.03 0.2) rosterLayout mainLayout isRoster
    where rosterLayout = mosaicLayout
          mainLayout = Grid
          isRoster = pidginRoster `Or` skypeRoster
          pidginRoster = And (ClassName "Pidgin") (Role "buddy_list")
          skypeRoster = Title $ skypeLogin ++ " - Skype™"
          skypeLogin = "elon00"
          -- next is preferable to last but too many window types - figure this out later
          -- skypeRoster = And (ClassName "Skype") (Not (Or (Role "ConversationsWindow") (Role "CallWindow")))
          -- Role Options should also be in main

mediaLayout = named "media" $ magnifiercz' 1.5 $ Tall nmaster delta ratio
	where
		nmaster = 1
		delta = 3/100
		ratio = 75/100

myLayout = avoidStruts $ 
    onWorkspace "gimp" (withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full) $
    onWorkspace "dev" (tiled ||| Accordion) $
    onWorkspace "media" (mediaLayout ||| Full) $
    onWorkspace "vm" (Full ||| tiled) $
	onWorkspace "im" imLayout $
    (tiled ||| Mirror tiled ||| Full ||| goldenStack)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

    goldenStack = StackTile nmaster delta golden

    golden = toRational (2/(1 + sqrt 5 :: Double))

    greedy = toRational (13/16)

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
-- http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#I_need_to_find_the_class_title_or_some_other_X_property_of_my_program 
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- *** SEE ALSO bin/xmonad_prop shell script
--
-- see xprop & xprop2
--
myManageHook = manageDocks <+> composeAll
    [ className =? "Rhythmbox"        --> doShift "media"
    , className =? "Audacious"        --> doShift "media"
	, className =? "Unity-2d-panel"	--> doIgnore
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "Do"             --> doIgnore
    , className =? "Truecrypt"        --> doFloat
    , className =? "Gnome-Screenshot" --> doIgnore
    , className =? "VirtualBox"       --> doShift "vm"
    , className =? "Thunderbird"   	  --> doShift "mail"
    , className =? "Update-manager"   --> doShift "mail"
    , (className =? "Nautilus" <&&> role =? " not found.") --> doFloat
    , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
    , className =? "Download"         --> doFloat
    , className =? "Extension"        --> doFloat
    , className =? "Unity-2d-panel"        --> doIgnore
    , className =? "Skype" <||> resource =? "skype" --> doShift "im"
    , className =? "Pidgin"         --> doF (W.shift "im")
	, className =? "emulator-arm" --> doFloat
    , isFullscreen                   --> doFullFloat
	, title =? "Spotify"			--> doFloat
    , resource  =? "desktop_window"   --> doIgnore ]
  where role = stringProperty "WM_WINDOW_ROLE"

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--

-- myLogHook = return ()

-- myLogHook :: X()
-- myLogHook = do ewmhDesktopsLogHook

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

-- height matches Ubuntu top Gnome panel
barHeight = "24"

myFont = "-misc-fixed-medium-r-normal-*-15-140-75-75-c-90-iso8859-2"

white = "#ffffff"
darkerGray = "#222222"
--  /usr/share/themes/Dust/gtk-2.0/gtkrc - bg - 323232
darkGray = "#383838"
lightGray = "#dddddd"
dustBackground = "#484840"
dustForeground = "#d9d4cc"
blue = "#4060ff"
green = "#00cc00"
black = "#000000"

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig {
                               XMonad.Prompt.font        = myFont
                             , XMonad.Prompt.bgColor     = darkGray
                             , XMonad.Prompt.fgColor     = white
                             , XMonad.Prompt.borderColor = white
}

myDzenPP h home = defaultPP
    { ppCurrent = formatLine urgentFg focusedBg normalFg . \wsId -> dropIx wsId
    , ppVisible = formatLine normalFg normalBg normalFg . \wsId -> dropIx wsId
    , ppHidden = wrap ("^i(" ++ iconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("^fg(" ++ hiddenFg ++ ")^bg()^p()^i(" ++ cornerIcon ++ ")") "^fg()^bg()^p()" . dropIx $ wsId
    , ppUrgent = formatLine urgentFg normalBg urgentFg . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = " "
    , ppTitle = dzenColor ("" ++ normalFg ++ "") "" . wrap "< " " >"
    , ppLayout = dzenColor ("" ++ normalFg ++ "") "" .
        (\x -> case x of
			"Full" -> "^fg(" ++ iconFg ++ ")^i(" ++ iconDir ++ "/layout-full.xbm)"
			"Tall" -> "^fg(" ++ iconFg ++ ")^i(" ++ iconDir ++ "/layout-tall.xbm)"
			_ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
    formatLine fg0 bg fg1 = wrap ("^fg(" ++ fg0 ++ ")^bg(" ++ bg ++ ")^p()^i(" ++ cornerIcon ++ ")^fg(" ++ fg1 ++ ")") "^fg()^bg()^p()"
    staticWs = ["web"]
    iconDir = home ++ "/.xmonad/icons"
    corner = "/corner.xbm"
    cornerIcon = iconDir ++ corner
    hiddenFg = darkGray
    normalFg = white
    normalBg = darkGray
    focusedBg = dustBackground
    urgentFg = "#0099ff"
    iconFg = "#777777"

quitWithWarning :: X ()
quitWithWarning = do
    let m = "confirm quit"
    s <- dmenu [m]
    when (m == s) (io exitSuccess)

getHostName :: IO String
getHostName = do
  host <- getSystemID
  return $ nodeName host
