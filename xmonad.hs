import XMonad
import XMonad.Hooks.DynamicLog

main = xmonad =<< xmobar defaultConfig
    { terminal    = "urxvtcd"
    , borderWidth = 1
    ,modMask = mod1Mask
    , focusFollowsMouse = False
    }
