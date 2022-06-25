import XMonad
import Data.Monoid
import System.Exit
import System.IO

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Run

import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders

import XMonad.Layout.ToggleLayouts

import XMonad.Config.Desktop

import XMonad.Actions.CycleWS

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Place
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal      = "cool-retro-term"
myBrowser       = "brave"
home            = "/home/eko"
myModMask       = mod4Mask
myBorderWidth   = 1
myNormalBorderColor  = "#c0c5ce"
myFocusedBorderColor = "#2aa899"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myWorkspaces    = [ "<fn=1> \xf303 </fn> "
                  , "<fn=2> \xf268 </fn>"
                  , "<fn=2> \xf392 </fn>"
                  , "<fn=2> \xf167 </fn>"
                  , "<fn=1> \xf11b </fn>"
                  , "<fn=3> \xf441 </fn>"
                  , "<fn=2> \xf799 </fn>"
                  , "<fn=1> \xf120 </fn>"
                  , "<fn=1> \xf1f8 </fn>"
                ]

myKeys = \c -> mkKeymap c $

        [ ("M-<Return>", spawn $ terminal c)
        , ("M-<Escape>", spawn "betterlockscreen -l -w dim")
        , ("M-<Space>", nextScreen)
        , ("M-a", spawn "rofi -show emoji -modi emoji")
        , ("M-b", spawn "brave")
        , ("M-d", spawn "rofi -show drun")
        , ("M-e", spawn "emacsclient -c -a 'emacs'")
        , ("M-g", spawn "/home/eko/.config/qtile/scripts/checkForGlava.sh alacritty")
        , ("M-i", spawn "clipmenu")
        , ("M-m", sendMessage ToggleLayout)
        , ("M-n", spawn "nautilus")
        , ("M-q", kill)
        , ("M-t", sendMessage NextLayout)

        , ("M-S-<Space>", shiftNextScreen)
        , ("M-S-d", spawn "rofi -show calc -no-show-match -no-sort")
        , ("M-S-g", spawn "/home/eko/.config/qtile/scripts/checkForGlava.sh glava")
        , ("M-S-r", spawn "xmonad --recompile; xmonad --restart")
        , ("M-S-g", spawn "/home/eko/.config/qtile/scripts/checkForGlava.sh glava")

    , ("M-S-C-x", io (exitWith ExitSuccess) )

    , ("M-C-<Esc>", spawn "systemctl suspend")
    , ("M-C-d", spawn "rofi -show window")

    , ("M1-p", spawn "pavucontrol")

    , ("M1-C-o", spawn "/home/eko/.config/qtile/scripts/picom-toggle.sh")
    , ("M1-C-t", spawn "xterm")

    , ("<Print>", spawn "flameshot full -p /home/eko/Pictures")

    , ("M-c", spawn "playerctl play-pause")
    , ("M-S-v", spawn "playerctl next")
    , ("M-S-x", spawn "playerctl previous")

    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-C-j", sendMessage Shrink)
    , ("M-C-k", sendMessage Expand)

    , ("M-h", windows W.focusDown)
    , ("M-l", windows W.focusUp)
    , ("M-S-h", windows W.swapDown)
    , ("M-S-l", windows W.swapUp)
    , ("M-C-h", sendMessage Shrink)
    , ("M-C-l", sendMessage Expand)
    -- Shrink/expand the master area
    -- (De)Increment the number of windows in the master area
    -- , ("M-,", sendMessage IncMasterN +1)
    -- , ("M-.", sendMessage IncMasterN -1)

    --  Reset the layouts on the current workspace to default
    -- , ("M-S-<Space>", setLayout $ myLayout.tiled)
    , ("M-S-<Return>", windows W.swapMaster)
    , ("M-m", windows W.focusMaster)
    , ("M-n", refresh)
    , ("M-S-t", withFocused $ windows . W.sink)

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    ]

    ++
    [("M-" ++ m ++ k, windows $ f i)
        | (i, k) <- zip (myWorkspaces) (map show([1 .. 9] ++ [0] :: [Int]))
        , (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]]

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

myLayout = smartBorders $ avoidStruts ( tiled ||| Mirror tiled ||| Full )
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "Alacritty"      --> hasBorder False
    , className =? "GLava"          --> hasBorder False
    , className =? "Alacritty"      --> doRectFloat (W.RationalRect 0.5 0 0.1 0.15)
    , className =? "GLava"          --> doRectFloat (W.RationalRect 1 1 0.1 0.1)
    ]

myEventHook = mempty

-- myLogHook dest = dynamicLogWithPP $ xmobarPP
--     { ppOutput = hPutStrLn dest
--     , ppTitle = xmobarColor "green" "" . shorten 50
--     }

myStartupHook = do
  spawnOnce "clipmenud"
  spawnOnce "tint2"
  spawnOnce "watch -n 60 feh --randomize --bg-fill ~/Pictures/wallpapers/Riced/* & disown"
  spawnOnce "emacs /usr/bin/emacs --daemon"
  spawnOnce "xset s off -dpms"
  spawnOnce "xinput --set-prop 'pointer:''Micro-Star INT'L CO., LTD. MSI GM41 Light Weight Wireless Mode Gaming Mouse' 'libinput Accel Profile Enabled' 0, 1'"
  spawnOnce "xinput --set-prop 'pointer:''Micro-Star INT'L CO., LTD. MSI GM41 Light Weight Wireless Mode Gaming Mouse' 'libinput Accel Speed' -0.2"
  spawnOnce "picom --config /home/eko/.config/picom/picom.conf"
  spawnOnce "dunst"

main = do
    xmproc <- spawnPipe "xmobar /home/eko/.config/xmonad/xmobarrc"
    barpipe <- spawnPipe "xmobar /home/eko/.config/xmonad/xmobarrcLeft"
    -- barpipe2 <- spawnPipe "xmobar /home/eko/.config/xmonad/xmobarrcRight"
    -- barpipe3 <- spawnPipe "xmobar /home/eko/.config/xmonad/xmobarrcCenter"
    xmonad $ docks $ ewmh def
        {
            terminal           = myTerminal,
            focusFollowsMouse  = myFocusFollowsMouse,
            clickJustFocuses   = myClickJustFocuses,
            borderWidth        = myBorderWidth,
            modMask            = myModMask,
            workspaces         = myWorkspaces,
            normalBorderColor  = myNormalBorderColor,
            focusedBorderColor = myFocusedBorderColor,
            keys               = myKeys,
            mouseBindings      = myMouseBindings,
            layoutHook         = myLayout,
            manageHook         = myManageHook,
            handleEventHook    = myEventHook,
            startupHook        = myStartupHook,
            logHook = dynamicLogWithPP $ xmobarPP {
                ppOutput = hPutStrLn xmproc
                , ppCurrent = xmobarColor "#95c7ae" "" . wrap
                            ("<box type=Bottom width=2 mb=2 color=#95c7ae>") "</box>"
                -- Visible but not current workspace
                , ppVisible = xmobarColor "#2aa899" ""
                -- Hidden workspace
                , ppHidden = xmobarColor "#2aa899" "" . wrap
                            ("<box type=Top width=2 mt=1 color=#2aa899>") "</box>"
                -- Hidden workspaces (no windows)
                , ppHiddenNoWindows = xmobarColor "#56b6c2" ""
                -- Title of active window
                , ppTitle = xmobarColor "#2aa899" "" . shorten 60
                -- Separator character
                , ppSep =  "<fc=#ffd47e> | </fc>"
                -- Urgent workspace
                , ppUrgent = xmobarColor "#ff5050" "" . wrap "!" "!"
            }
        }

-- defaults = def {
--       -- simple stuff
--         terminal           = myTerminal,
--         focusFollowsMouse  = myFocusFollowsMouse,
--         clickJustFocuses   = myClickJustFocuses,
--         borderWidth        = myBorderWidth,
--         modMask            = myModMask,
--         workspaces         = myWorkspaces,
--         normalBorderColor  = myNormalBorderColor,
--         focusedBorderColor = myFocusedBorderColor,

        -- keys               = myKeys,
        -- mouseBindings      = myMouseBindings,

    --     layoutHook         = myLayout,
    --     manageHook         = myManageHook,
    --     handleEventHook    = myEventHook,
    --     logHook = dynamicLogWithPP $ def { ppOutput = hPutStrLn barpipe }
    --     startupHook        = myStartupHook
    -- }
