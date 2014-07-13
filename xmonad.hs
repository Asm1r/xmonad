import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run

import XMonad.Layout.NoBorders

main = xmonad =<< xmobar defaultConfig
    { terminal    = "urxvtcd"
    ,borderWidth = 1
    ,modMask = mod1Mask
    ,focusFollowsMouse = False
    ,manageHook = myManageHook
    ,layoutHook = smartBorders $ myLayout 
    }

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall 1 (3/100) (3/5) 

myManageHook = composeAll . concat $
                [ [ className =? c --> doFloat | c <- floats]
                , [ resource =? r --> doIgnore | r <- ignore]]
 where floats = ["MPlayer", "Gimp", "mpv"]
       ignore = ["panel", "trayer", "xmobar"]
