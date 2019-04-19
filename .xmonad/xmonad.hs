import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Cursor
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import Graphics.X11.ExtraTypes.XF86
import XMonad

myStartupHook :: X ()
myStartupHook = do
        setWMName "LG3D"

main = do
    xmproc <- spawnPipe ( "xmobar " ++ "~/.xmonad/.mobarrc")

    xmonad $ def
        { manageHook = manageDocks <+> manageHook def
        , handleEventHook    = handleEventHook def <+> docksEventHook
        , layoutHook = avoidStruts  $  layoutHook def
        , logHook = dynamicLogWithPP xmobarPP     
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 100
                        , ppCurrent = xmobarColor "red" "" 
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , terminal = "urxvt"
        , startupHook = myStartupHook
        , borderWidth = 2
        , normalBorderColor = "#cccccc"
        , focusedBorderColor = "#ff0000"
        } `additionalKeys`
        [ 
            ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off"), 
            ((0, xK_Print), spawn "scrot -u screen_%Y-%m-%dT%H-%M-%S.png -e 'mv $f ~/Pictures/' "),
            ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5-"),
            ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5+"),
            ((0, xF86XK_AudioMute), spawn "amixer set Master toggle"),
            ((mod4Mask, xF86XK_AudioMute), spawn "pavucontrol"),
            ((mod4Mask .|. shiftMask .|. controlMask, xK_q), spawn "/sbin/shutdown -h now"),
            ((0, xF86XK_Search), spawn "google-chrome")
        ]

