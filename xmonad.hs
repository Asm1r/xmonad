import XMonad hiding((|||))
import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.CycleWS
import XMonad.Config.Desktop
import XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.Dishes
import XMonad.Layout.Dwindle
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts


import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

myTerminal :: String
myTerminal = "urxvtcd"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myBorderWidth :: Dimension
myBorderWidth = 1

myModMask :: KeyMask
myModMask = mod1Mask

myStartupHook :: X ()
myStartupHook = setWMName "LG3D"


myLayout =
  smartBorders $ avoidStruts $ tiled ||| Mirror tiled ||| full ||| tabbed
 where
  tiled  = name "tiled" $ Tall 1 (3 / 100) (3 / 5)
  tabbed = name "tabbed" simpleTabbed
  full   = name "full" Full
  name n = renamed [Replace n] . smartBorders

myManageHook =
  composeAll
    . concat
    $ [ [ className =? c --> doFloat | c <- floats ]
      , [ resource =? r --> doIgnore | r <- ignore ]
      , [ className =? c --> doShift "www" | c <- web ]
      , [ className =? c --> doShift "im" | c <- im ]
      , [ className =? c --> doShift "art" | c <- art ]
      , [ className =? c --> doShift "doc" | c <- doc ]
      , [ className =? c --> doShift "lab" | c <- lab ]
      ]
 where
  floats = ["MPlayer", "Gimp", "mpv", "Sxiv"]
  ignore = ["panel", "trayer"]
  im     = ["Thunderbird", "Sylpheed"]
  web    = ["Luakit"]
  doc    = ["Zathura", "Evince"]
  art    = ["Gimp", "Inkscape", "krita"]
  lab    = ["matlab"]


myWorkspaces =
  ["www", "tty", "dev", "dev", "art", "im", "lab", "misc", "doc", "null"]


defaults =
  def { terminal          = myTerminal
      , modMask           = myModMask
      , borderWidth       = myBorderWidth
      , focusFollowsMouse = myFocusFollowsMouse
      , layoutHook        = myLayout
      , startupHook       = myStartupHook
      , manageHook        = myManageHook
      , XMonad.workspaces = myWorkspaces
      }
    `additionalKeys` [ ( (noModMask, xF86XK_AudioMute)
                       , spawn "pactl set-sink-volume 0 0"
                       )
                     , ( (noModMask, xF86XK_AudioRaiseVolume)
                       , spawn "pactl set-sink-volume 0 +5%"
                       )
                     , ( (noModMask, xF86XK_AudioLowerVolume)
                       , spawn "pactl set-sink-volume 0 -5%"
                       )
                     , ((myModMask, xK_a), moveTo Next EmptyWS)
                     , ((myModMask, xK_m), sendMessage $ JumpToLayout "full")
                     , ((myModMask, xK_t), sendMessage $ JumpToLayout "tiled")
                     , ( (myModMask, xK_n)
                       , namedScratchpadAction scratchpads "ncmpcpp"
                       )
                     , ( (myModMask, xK_v)
                       , namedScratchpadAction scratchpads "viber"
                       )
                     , ( (myModMask, xK_h)
                       , namedScratchpadAction scratchpads "htop"
                       )
                     , ((mod4Mask, xK_p), spawn "mpc toggle")
                     , ((mod4Mask, xK_x), spawn "pactl set-sink-volume 0 +5%")
                     , ((mod4Mask, xK_z), spawn "pactl set-sink-volume 0 -5%")
                     , ((mod4Mask, xK_s), spawn "sudo pm-suspend")
                     , ((myModMask, xK_Escape), toggleWS' ["NSP"])
                     , ((myModMask .|. shiftMask, xK_l), spawn "i3lock")
                     ]

scratchpads =
  [ NS "ncmpcpp"
       (myTerminal ++ "-e ncmpcpp")
       (title =? "ncmpcpp")
       (customFloating someCenter)
  , NS "viber" "viber" (className =? "ViberPC") (customFloating someCenter)
  , NS "htop"
       (myTerminal ++ " -e htop")
       (title =? "htop")
       (customFloating someCenter)
  ]
  where someCenter = W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks $ defaults
    { manageHook      = manageDocks
      <+> namedScratchpadManageHook scratchpads
      <+> myManageHook
    , handleEventHook = handleEventHook def <+> docksEventHook
    , logHook         = dynamicLogWithPP
      . namedScratchpadFilterOutWorkspacePP
      $ xmobarPP { ppOutput  = hPutStrLn xmproc
                 , ppCurrent = xmobarColor "#d0d0d0" "#151515"
                 , ppUrgent  = xmobarColor "#202020" "#ac4142"
                 , ppVisible = xmobarColor "#90a959" "#151515"
                 }
    }

