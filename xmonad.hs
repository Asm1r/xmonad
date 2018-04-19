import XMonad hiding((|||))
import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.CycleWS
import XMonad.Config.Desktop
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)

import XMonad.Layout.Dishes
import XMonad.Layout.Dwindle
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.Spacing

import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

myTerminal :: String
myTerminal = "urxvtcd"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myBorderWidth :: Dimension
myBorderWidth = 2

myModMask :: KeyMask
myModMask = mod1Mask

myStartupHook :: X ()
myStartupHook = setWMName "LG3D"

myFocusedBorderColor :: String
myFocusedBorderColor = "#5900b3"

myNormalBorderColor :: String
myNormalBorderColor = "#000000"

myWorkspaces :: [String]
myWorkspaces = ["一", "二", "三", "四", "五", "六", "七", "八", "九", "十"]

myLayout =
  smartBorders
    $   maximize
    $   minimize
    $   avoidStruts
    $   tiled
    ||| Mirror tiled
    ||| full
    ||| tabbed
 where
  tiled  = name "tiled" $ smartSpacing 3 $ Tall 1 (3 / 100) (3 / 5)
  tabbed = name "tabbed" simpleTabbed
  full   = name "full" Full
  name n = renamed [Replace n] . smartBorders

myManageHook =
  composeAll
    . concat
    $ [ [ className =? c --> doFloat | c <- floats ]
      , [isFullscreen --> doFullFloat]
      , [ resource =? r --> doIgnore | r <- ignore ]
      , [ className =? c --> doShift "一" | c <- web ]
      , [ className =? c --> doShift "六" | c <- im ]
      , [ className =? c --> doShift "五" | c <- art ]
      , [ className =? c --> doShift "九" | c <- doc ]
      , [ className =? c --> doShift "七" | c <- lab ]
      , [ className =? c --> doShift "八" | c <- media ]
      ]
 where
  floats = []
  ignore = ["panel", "trayer"]
  im     = ["Thunderbird", "Sylpheed"]
  web    = ["Luakit", "qutebrowser"]
  doc    = ["Zathura", "Evince"]
  art    = ["Gimp", "Inkscape", "krita"]
  lab    = ["matlab"]
  media  = ["Kodi"]

defaults =
  def { terminal           = myTerminal
      , modMask            = myModMask
      , borderWidth        = myBorderWidth
      , focusFollowsMouse  = myFocusFollowsMouse
      , layoutHook         = myLayout
      , startupHook        = myStartupHook
      , manageHook         = myManageHook
      , workspaces         = myWorkspaces
      , focusedBorderColor = myFocusedBorderColor
      , normalBorderColor  = myNormalBorderColor
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
                     , ( (myModMask, xK_n)
                       , moveTo Next EmptyWS
                       )
                     --, ((myModMask, xK_s), withFocused $ windows . W.sink)
                     , ((myModMask, xK_m), sendMessage $ JumpToLayout "full")
                     , ((myModMask, xK_t), sendMessage $ JumpToLayout "tiled")
                     , ((myModMask, xK_b), sendMessage $ JumpToLayout "tabbed")
                     , ((myModMask, xK_x), sendMessage ToggleStruts)
                     , ( (mod4Mask, xK_j)
                       , namedScratchpadAction scratchpads "viber"
                       )
                     , ( (mod4Mask, xK_h)
                       , namedScratchpadAction scratchpads "htop"
                       )
                     , ( (mod4Mask, xK_n)
                       , namedScratchpadAction scratchpads "ncmpcpp"
                       )
                     , ( (mod4Mask, xK_m)
                       , namedScratchpadAction scratchpads "neomutt"
                       )
                     , ( (mod4Mask, xK_y)
                       , namedScratchpadAction scratchpads "pulsemixer"
                       )
                     , ( (mod4Mask, xK_l)
                       , namedScratchpadAction scratchpads "python3"
                       )
                     , ((mod4Mask, xK_p), spawn "mpc toggle")
                     , ((mod4Mask, xK_x), spawn "pactl set-sink-volume 0 +5%")
                     , ((mod4Mask, xK_z), spawn "pactl set-sink-volume 0 -5%")
                     , ((mod4Mask, xK_s), spawn "sudo pm-suspend")
                     , ((myModMask, xK_Escape), toggleWS' ["NSP"])
                     , ((myModMask .|. shiftMask, xK_l), spawn "slock")
                     ]

scratchpads =
  [ NS "viber" "viber" (className =? "ViberPC") (customFloating someCenter)
  , genericTermFloat "htop"
  , genericTermFloat "ncmpcpp"
  , genericTermFloat "neomutt"
  , genericTermFloat "pulsemixer"
  , genericTermFloat "python3"
  ]
 where
  someCenter = W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)
  genericTermFloat x =
    NS x (myTerminal ++ " -e " ++ x) (title =? x) (customFloating someCenter)

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks $ fullscreenSupport $ defaults
    { manageHook      = manageDocks
      <+> namedScratchpadManageHook scratchpads
      <+> myManageHook
    , handleEventHook = handleEventHook def
      <+> docksEventHook
      <+> fullscreenEventHook
    , logHook         = dynamicLogWithPP
      . namedScratchpadFilterOutWorkspacePP
      $ xmobarPP { ppOutput  = hPutStrLn xmproc
                 , ppCurrent = xmobarColor "#d0d0d0" "#151515"
                 , ppUrgent  = xmobarColor "#202020" "#ac4142"
                 , ppVisible = xmobarColor "#90a959" "#151515"
                 }
    }

