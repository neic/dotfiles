import XMonad
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

xF86AudioRaiseVolume,xF86AudioLowerVolume, xF86AudioMute,xF86AudioPlay,xF86AudioStop,xF86AudioNext,xF86AudioPrev :: KeySym
xF86AudioRaiseVolume = 0x1008ff13
xF86AudioLowerVolume = 0x1008ff11
xF86AudioMute = 0x1008ff12
xF86AudioPlay = 0x1008ff14
xF86AudioStop = 0x1008ff15
xF86AudioNext = 0x1008ff17
xF86AudioPrev = 0x1008ff16


main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
    	{ workspaces = ["1","2","3","4","5","6","7","8","9:Musik"]
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = smartBorders $ avoidStruts $  layoutHook defaultConfig
        , modMask = mod4Mask
        , terminal = "urxvt"
	, borderWidth = 1
	, normalBorderColor = "#1f1f1f"
	, focusedBorderColor = "#dc8cc3"
	, logHook = dynamicLogWithPP xmobarPP
          	  { ppOutput = hPutStrLn xmproc
		  , ppHidden = xmobarColor "#d0bf8f" ""
		  , ppCurrent = xmobarColor "#dc8cc3" "" . wrap "[" "]"
		  , ppUrgent = xmobarColor "#dca3a3" "" . wrap "*" "*"
		  , ppLayout = xmobarColor "#ffffff" ""
                  , ppTitle = xmobarColor "#bfebbf" "" . shorten 50
                  }
        } `additionalKeys`
        [ ((mod4Mask, xK_p),    spawn "exe=`dmenu | yeganesh` && eval \"exec $exe\"")

        -- alsa volume controls
        , ((0, xF86AudioMute), spawn "amixer -q set PCM toggle")
        , ((0, xF86AudioRaiseVolume), spawn "amixer -q set PCM 2+")
        , ((0, xF86AudioLowerVolume), spawn "amixer -q set PCM 2-")

        -- cmus controls
        , ((0, xF86AudioPlay), spawn "cmus-remote -u")
        , ((0, xF86AudioStop), spawn "cmus-remote -s")
        , ((0, xF86AudioNext), spawn "cmus-remote -n")
        , ((0, xF86AudioPrev), spawn "cmus-remote -r")

        
        , ((mod4Mask, xK_e),    spawn "emacsclient -c")
        , ((mod4Mask, xK_c),    spawn "firefox")
        , ((mod4Mask, xK_o),    spawn "libreoffice")
        , ((mod4Mask, xK_r),    spawn "evince")
        ]