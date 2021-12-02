import XMonad.Hooks.SetWMName
import XMonad.Actions.SpawnOn
import XMonad.Util.SpawnOnce
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Util.Cursor
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import Graphics.X11.ExtraTypes.XF86
import XMonad

import qualified Data.Map as M         -- haskell modules
import qualified XMonad.StackSet as W

myStartupHook :: X ()
myStartupHook = do
        setWMName "LG3D"
        spawnOnOnce "workspace1" "google-chrome"
        spawnOnOnce "workspace1" "urxvt"

main = do
    xmproc <- spawnPipe ( "xmobar " ++ "~/.xmonad/.mobarrc")

    xmonad $ def
        { manageHook = manageDocks <+> manageHook def
        , handleEventHook    = handleEventHook def <+> docksEventHook
        , layoutHook = smartBorders $
                    avoidStruts $
                    layoutHook def
        , logHook = dynamicLogWithPP xmobarPP     
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 100
                        , ppCurrent = xmobarColor "red" "" 
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , mouseBindings = myMouseBindings
        , terminal = "urxvt"
        , startupHook = myStartupHook
        , borderWidth = 2
        , normalBorderColor = "#303030"
        , focusedBorderColor = "#b00000"
        } `additionalKeys`
        [ 
            ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off"), 
            ((0, xK_Print), spawn "scrot -u screen_%Y-%m-%dT%H-%M-%S.png -e 'mv $f ~/Pictures/' "),
            ((mod4Mask, xK_s), spawn "scrot -u screen_%Y-%m-%dT%H-%M-%S.png -e 'mv $f ~/Pictures/' "),
            ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5-"),
            ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5+"),
            ((0, xF86XK_AudioMute), spawn "amixer set Master toggle"),
            ((mod4Mask, xF86XK_AudioMute), spawn "pavucontrol"),
            ((mod4Mask .|. shiftMask .|. controlMask, xK_q), spawn "/sbin/shutdown -h now"),
            ((0, xF86XK_Search), spawn "google-chrome")
        ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Sink the window back to the grid.
    , ((mod4Mask .|. controlMask, button1), (\w -> withFocused $ windows . W.sink))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

