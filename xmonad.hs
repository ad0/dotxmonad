import XMonad
import XMonad.Config.Azerty
import XMonad.Util.EZConfig

import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Util.Run
import XMonad.Actions.CycleWS

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive

import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.SimpleFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import System.IO
import System.Exit


myTerminal = "urxvt"
myModMask = mod4Mask

myWorkspaces = map show [1..6]

myXmonadBar = "dzen2 -x '0' -y '0' -h '24' -w '840' -ta 'l' -fg '#FFFFFF' -bg '#5c5c5c'"
myStatusBar = "conky -c /home/ad/.xmonad/conky_dzen | dzen2 -x '840' -y '0' -w '840' -h '24' -ta 'r' -fg '#FFFFFF' -bg '#5c5c5c'"
myBitmapsDirs = "/home/ad/.xmonad/bitmaps_dzen"

main = do
  dzenLeftBar <- spawnPipe myXmonadBar
  dzenRightBar <- spawnPipe myStatusBar
  xmonad $ azertyConfig
    { terminal              = myTerminal
    , workspaces            = myWorkspaces
    , modMask               = myModMask
    , layoutHook            = myLayoutHook
    , logHook               = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddd
    , normalBorderColor     = myNormalBorderColor
    , focusedBorderColor    = myFocusedBorderColor
    , borderWidth           = myBorderWidth
    } `additionalKeys` myKeys

-- indirection to allow customization : onWorkspaces ["1:main"] aLayout $ customLayout
myLayoutHook = customLayout

customLayout = avoidStruts $
  smartBorders tiled ||| Mirror tiled ||| noBorders Full ||| simpleFloat
  where
    tiled = ResizableTall 1 (2/100) (1/2) []

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
  { ppCurrent         = dzenColor "#1e90ff" "#5c5c5c" . pad
  , ppVisible         = dzenColor "#000000" "#5c5c5c" . pad
  , ppHidden          = dzenColor "#000000" "#5c5c5c" . pad
  , ppHiddenNoWindows = dzenColor "#7b7b7b" "#5c5c5c" . pad
  , ppUrgent          = dzenColor "#ff0000" "#5c5c5c" . pad
  , ppWsSep           = " "
  , ppSep             = "  |  "
  , ppLayout          = dzenColor "#1e90ff" "#5c5c5c" .
    (\x -> case x of
      "ResizableTall"        -> "^i(" ++ myBitmapsDirs ++ "/tall.xbm)"
      "Mirror ResizableTall" -> "^i(" ++ myBitmapsDirs ++ "/mtall.xbm)"
      "Full"                 -> "^i(" ++ myBitmapsDirs ++ "/full.xbm)"
      "Simple Float"         -> "~"
      _                      -> x
    )
  , ppTitle           = (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
  , ppOutput          = hPutStrLn h
  }

myNormalBorderColor = "#5c5c5c"
myFocusedBorderColor = "#1e90ff"
myBorderWidth = 2

myKeys =
  [ ((mod4Mask,                xK_p             ), runOrRaisePrompt defaultXPConfig)
  , ((mod4Mask,                xK_o             ), spawn "firefox")
  , ((mod4Mask .|. shiftMask,  xK_z             ), spawn "xscreensaver-command -lock")
  , ((mod4Mask,                xK_b             ), sendMessage ToggleStruts)
  , ((mod4Mask,                xK_F1            ), spawn "xdotool key XF86AudioPrev")
  , ((mod4Mask,                xK_F2            ), spawn "xdotool key XF86AudioNext")
  , ((mod4Mask,                xK_F3            ), spawn "xdotool key XF86AudioPlay")
  , ((mod4Mask,                xK_F6            ), spawn "xbacklight -dec 10")
  , ((mod4Mask,                xK_F7            ), spawn "xbacklight -inc 10")
  , ((mod4Mask,                xK_F8            ), spawn "amixer sset Master toggle")
  , ((mod4Mask,                xK_F9            ), spawn "amixer -c 1 sset Master 2%-")
  , ((mod4Mask,                xK_F10           ), spawn "amixer -c 1 sset Master 2%+")
  ]

