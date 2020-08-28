-- import Control.Monad.Trans (liftIO)
import Control.Monad (liftM)
import System.Process
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)

---- xprop WM_CLASS
myManageHook :: ManageHook
myManageHook = composeAll
               [ className =? "Xmessage" --> doFloat
               , className =? "Clock" --> doFloat
--               , className =? "MPlayer" --> (doF W.sink)
               ]

myStartupHook = composeAll
                [ runShellCmd "~/bin/xmonad_startup.sh"
                , setWMName "LG3D"
                ]

myModMask = mod4Mask

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig
       { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
       , startupHook = myStartupHook <+> startupHook defaultConfig
       , layoutHook = avoidStruts  $  layoutHook defaultConfig
       , terminal = "urxvt"
       , modMask = myModMask
       }
       -- $ xmodmap
       `additionalKeys`
       [
       -- show/hide Xmobar
         ((myModMask, xK_b), sendMessage ToggleStruts)
       -- volume
       , ((myModMask, xK_m), setVolume $ 0)
       , ((myModMask, xK_Down), setVolume $ -4)
       , ((myModMask, xK_Up), setVolume $ 4)
       -- brightness
       , ((myModMask, xK_Left), setBrightness $ -1)
       , ((myModMask, xK_Right), setBrightness $ 1)
--       , ((myModMask, xF86XK_AudioMute), setVolume $ 0)
--       , ((myModMask, xF86XK_AudioLowerVolume), setVolume $ -4)
--       , ((myModMask, xF86XK_AudioRaiseVolume), setVolume $ 4)
       -- mplayer
       , ((myModMask, xK_z), mplayerPause)
       , ((myModMask, xK_s), mplayerNext)
       , ((myModMask, xK_a), mplayerPrev)
       , ((myModMask, xK_x), mplayerSeek $ -15)
       , ((myModMask, xK_c), mplayerSeek $ 15)
       ]
       `additionalKeysP`
       [ ("<XF86AudioLowerVolume>", setVolume $ -4)
       , ("<XF86AudioRaiseVolume>", setVolume $ 4)
       , ("<XF86AudioMute>", setVolume $ 0)
--       , ("<F12>", mute  >> return ())
--       , ("<F4>", ....
       ]

--------
--------

runShellCmd :: [Char] -> X ()
runShellCmd cmd = spawnPipe cmd >> return ()

-- todo: handle file I/O and arithmetic
--       with haskell rather than bash and bc
setBrightness :: Int -> X ()
setBrightness inc = runShellCmd bCmd
  where
    brightnessPath = "/sys/devices/pci0000:00/0000:00:01.0/0000:01:00.0/backlight/acpi_video0/brightness"
    bCmd
      | (inc > 0) = "echo $(cat " ++ brightnessPath ++ ") + 1 | bc > " ++ brightnessPath
      | otherwise = "echo $(cat " ++ brightnessPath ++ ") - 1 | bc > " ++ brightnessPath

setVolume :: Int -> X ()
setVolume percent = runShellCmd volCmd
  where
    baseCmd = "amixer sset 'Master' "
    volCmd
      | (percent == 0) = baseCmd ++ "0%"
      | abs percent == percent = baseCmd ++ (show $ abs percent) ++ "%+"
      | otherwise = baseCmd ++ (show $ abs percent)  ++ "%-"

-------------

mplayerPause :: X ()
mplayerPause = runShellCmd "echo pause > /tmp/mplayer.fifo"

mplayerSeek :: Int -> X ()
mplayerSeek sec = runShellCmd cmd
  where
    cmd
      | sec < 0 = "echo \"seek -"++(show $ abs sec)++"\" > /tmp/mplayer.fifo"
      | otherwise = "echo \"seek +"++(show $ abs sec)++"\" > /tmp/mplayer.fifo"

mplayerNext :: X ()
mplayerNext = runShellCmd "echo \"pt_step +1\" > /tmp/mplayer.fifo"

mplayerPrev :: X ()
mplayerPrev = runShellCmd "echo \"pt_step -1\" > /tmp/mplayer.fifo"

--mute :: X ()
--mute = liftIO
  --     $ (createProcess $ shell "amixer sset 'Master' 0%")
