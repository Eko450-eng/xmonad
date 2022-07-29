--  ╭──────────────────────────────────────────────────────────╮
--  │ Imports                                                  │
--  ╰──────────────────────────────────────────────────────────╯
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
import XMonad.Layout.ThreeColumns

import XMonad.Layout.ToggleLayouts

import XMonad.Config.Desktop

import XMonad.Actions.CycleWS

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Place
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import Colors.DoomOne

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

--  ╭──────────────────────────────────────────────────────────╮
--  │ Base variables                                           │
--  ╰──────────────────────────────────────────────────────────╯

myTerminal      = "kitty"
myBrowser       = "brave"
home            = "/home/eko"
myModMask       = mod4Mask
myBorderWidth   = 1
myNormalBorderColor  = "#c0c5ce"
myFocusedBorderColor = "#2aa899"

--  ╭──────────────────────────────────────────────────────────╮
--  │ Mouse config                                             │
--  ╰──────────────────────────────────────────────────────────╯

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

--  ╭──────────────────────────────────────────────────────────╮
--  │ Workspaces                                               │
--  ╰──────────────────────────────────────────────────────────╯

myWorkspaces = [ "\xf268" , "\xf121" , "\xf392" , "\xf167" , "\xf11b" , "\xf120" , "\xf799" , "\xf0e0" , "\xf1f8" ]                                   
--  ╭──────────────────────────────────────────────────────────╮
--  │ myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7",      │
--  │  "8", "9"]                                               │
--  ╰──────────────────────────────────────────────────────────╯

--  ╭──────────────────────────────────────────────────────────╮
--  │ Keybinds                                                 │
--  ╰──────────────────────────────────────────────────────────╯

myKeys = \c -> mkKeymap c $

--  ╭──────────────────────────────────────────────────────────╮
--  │ Super keys                                               │
--  ╰──────────────────────────────────────────────────────────╯
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
        , ("M-o", spawn "pactl -- set-sink-volume 0 -5%")
        , ("M-p", spawn "pactl -- set-sink-volume 0 +5%")
        , ("M-q", kill)
        , ("M-r", spawn "neovide")
        , ("M-s", spawn "deepin-screenshot")
        , ("M-t", sendMessage NextLayout)
        , ("M-z", spawn "/home/eko/.config/fish/functions/toggleAudio.sh")

--  ╭──────────────────────────────────────────────────────────╮
--  │ Super Shift                                              │
--  ╰──────────────────────────────────────────────────────────╯

        , ("M-S-<Space>", shiftNextScreen)
        , ("M-S-d", spawn "rofi -show calc -no-show-match -no-sort")
        , ("M-S-g", spawn "/home/eko/.config/qtile/scripts/checkForGlava.sh glava")
        , ("M-S-o", spawn "pactl -- set-sink-volume 0 -10%")
        , ("M-S-p", spawn "pactl -- set-sink-volume 0 +10%")
        , ("M-S-r", spawn "killall polybar; xmonad --recompile; xmonad --restart")

--  ╭──────────────────────────────────────────────────────────╮
--  │ Super shift control                                      │
--  ╰──────────────────────────────────────────────────────────╯

    , ("M-S-C-x", io (exitWith ExitSuccess) )

--  ╭──────────────────────────────────────────────────────────╮
--  │ Super Control                                            │
--  ╰──────────────────────────────────────────────────────────╯

    , ("M-C-<Esc>", spawn "systemctl suspend")
    , ("M-C-d", spawn "rofi -show window")

--  ╭──────────────────────────────────────────────────────────╮
--  │ Alt                                                      │
--  ╰──────────────────────────────────────────────────────────╯

    , ("M1-p", spawn "pavucontrol")
    , ("M1-w", spawn "killall polybar")
    , ("M1-r", spawn "ps cax | grep polybar ; if ! [ $? -eq 0 ]; then polybar; fi")

--  ╭──────────────────────────────────────────────────────────╮
--  │ Alt control                                              │
--  ╰──────────────────────────────────────────────────────────╯

    , ("M1-C-o", spawn "/home/eko/.config/qtile/scripts/picom-toggle.sh")
    , ("M1-C-t", spawn "xterm")

--  ╭──────────────────────────────────────────────────────────╮
--  │ Multimedia                                               │
--  ╰──────────────────────────────────────────────────────────╯

    , ("<Print>", spawn "flameshot full -p /home/eko/Pictures")

    , ("M-c", spawn "playerctl play-pause")
    , ("M-S-v", spawn "playerctl next")
    , ("M-S-x", spawn "playerctl previous")

--  ╭──────────────────────────────────────────────────────────╮
--  │ Move windows                                             │
--  ╰──────────────────────────────────────────────────────────╯
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

myLayout = avoidStruts
           $ smartBorders (threeCol ||| tiled ||| Mirror tiled ||| Full )
  where
     threeCol   = ThreeCol nmaster delta ratio
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



myStartupHook = do
  spawn "ps cax | grep clipmenud ; if ! [ $? -eq 0 ]; then clipmenud; fi"
  spawn "ps cax | grep polybar ; if ! [ $? -eq 0 ]; then polybar; fi"
  spawnOnce "wallpaperChanger"
  spawnOnce "emacs /usr/bin/emacs --daemon"
  spawnOnce "xset s off -dpms"
  spawnOnce "/home/eko/.config/qtile/scripts/mouseAccel.sh"
  spawnOnce "picom --config /home/eko/.config/picom/picom.conf"
  spawnOnce "dunst"

main = do
    -- xmproc0 <- spawnPipe "xmobar -x 2 /home/eko/.config/xmonad/xmobarrc0"
    -- xmproc1 <- spawnPipe "xmobar -x 2 /home/eko/.config/xmonad/xmobarrc1"
    -- xmproc2 <- spawnPipe "xmobar -x 2 /home/eko/.config/xmonad/xmobarrc2"
    xmonad $ docks $ ewmhFullscreen $ ewmh def
        { terminal           = myTerminal
        , focusFollowsMouse  = myFocusFollowsMouse
        , clickJustFocuses   = myClickJustFocuses
        , borderWidth        = myBorderWidth
        , modMask            = myModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , keys               = myKeys
        , mouseBindings      = myMouseBindings
        , layoutHook         = myLayout
        , manageHook         = myManageHook
        , handleEventHook    = myEventHook
        , startupHook        = myStartupHook
        -- , logHook            = dynamicLogWithPP $ xmobarPP
        --         { ppOutput = hPutStrLn xmproc0
        --         , ppCurrent = xmobarColor "#95c7ae" "" . wrap
        --                     ("<box type=Bottom width=2 mb=2 color=#95c7ae>") "</box>"
        --         , ppVisible = xmobarColor "#2aa899" ""
        --         , ppHidden = xmobarColor "#2aa899" "" . wrap
        --                     ("<box type=Top width=2 mt=1 color=#2aa899>") "</box>"
        --         , ppHiddenNoWindows = xmobarColor "#56b6c2" ""
        --         , ppTitle = xmobarColor "#2aa899" "" . shorten 60
        --         , ppSep =  "<fc=#ffd47e> | </fc>"
        --         , ppUrgent = xmobarColor "#ff5050" "" . wrap "!" "!"
        --         , ppOrder = \(ws:l:_:_) -> [ws,l]
        --     }
        }
