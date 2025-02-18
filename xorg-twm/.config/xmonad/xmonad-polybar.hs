import Control.Monad (liftM2)
import Data.Ratio ((%))
import XMonad
import XMonad.Actions.CycleWS
-- import XMonad.Hooks.ShowWName -- TEST IT
import XMonad.Actions.EasyMotion (EasyMotionConfig (..), selectWindow, textSize)
import XMonad.Actions.Minimize
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName (showWName)
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
-- import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.Hacks (javaHack, trayAbovePanelEventHook, trayPaddingEventHook, trayPaddingXmobarEventHook, trayerAboveXmobarEventHook, trayerPaddingXmobarEventHook, windowedFullscreenFixEventHook)
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce (spawnOnce)

-- import XMonad.Actions.ShowText

main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . docks
    $ myConfig

myConfig =
  def
    { modMask = mod4Mask, -- Rebind Mod to the Super key
      layoutHook = myLayout, -- Use custom layouts
      manageHook = myManageHook, -- Match on certain windows
      handleEventHook = windowedFullscreenFixEventHook <> trayerPaddingXmobarEventHook,
      startupHook = myStartupHook,
      workspaces = myWorkspaces,
      terminal = myTerminal,
      borderWidth = 3,
      normalBorderColor = "#1b2b34",
      focusedBorderColor = "#0080FF"
    }
    `additionalKeysP` myKeys
    `removeKeysP` ["M-S-q"]

myKeys :: [(String, X ())]
myKeys =
  [ ("M-S-r", spawn "xmonad --recompile && xmonad --restart"),
    ("M-C-r", spawn "xmonad --restart"),
    ("M-a", spawn "rofi -show drun -show-icons"),
    ("M-w", spawn "rofi -show window -show-icons"),
    ("M-s", spawn "rofi -show run"),
    ("M-e", spawn "emacsclient -c -a 'emacs'"),
    ("M-<Return>", spawn "alacritty -e tmux new-session -A -s 'Main'")
  ]
    ++ [ ("M-.", nextWS), -- %! Switch to Next WS
         ("M-,", prevWS), -- %! Switch to Prev WS
         ("M-S-.", shiftToNext >> nextWS), -- %! Switch Window to Next WS
         ("M-S-,", shiftToPrev >> prevWS), -- %! Switch Window to Next WS
         ("M-C-.", shiftToNext), -- %! Move Window to Next WS
         ("M-C-,", shiftToPrev), -- %! Move Window to Prev WS
         ("M-<Backspace>", toggleWS), -- %! Switch to Last Used WS
         ("M-'", moveTo Next $ hiddenWS :&: Not emptyWS), -- %! Switch to Next Non-Empty WS
         ("M-;", moveTo Prev $ hiddenWS :&: Not emptyWS), -- %! Switch to Prev Non-Empty WS
         ("M-S-'", shiftTo Next $ hiddenWS :&: Not emptyWS),
         ("M-S-;", shiftTo Prev $ hiddenWS :&: Not emptyWS),
         ("M-n", refresh), -- What it does?

         -- resizing the master/slave ratio
         ("M-l", sendMessage Shrink), -- %! Shrink the master area
         ("M-h", sendMessage Expand), -- %! Expand the master area
         -- floating layer support
         ("M-t", withFocused $ windows . W.sink), -- %! Push window back into tiling

         -- EasyMotion
         ( "M-o",
           selectWindow
             def
               { txtCol = "Green",
                 cancelKey = xK_Escape,
                 emFont = "xft: Sans-40",
                 overlayF = textSize
               }
             >>= (`whenJust` windows . W.focusWindow)
         ),
         ( "M-x",
           selectWindow
             def
               { txtCol = "Red",
                 cancelKey = xK_Escape,
                 emFont = "xft: Sans-40",
                 overlayF = textSize
               }
             >>= (`whenJust` killWindow)
         ),
         -- clients
         ("M-q", kill),
         ("M-i", withFocused minimizeWindow),
         ("M-S-i", withLastMinimized maximizeWindowAndFocus),
         ("M-u", withFocused (sendMessage . maximizeRestore)),
         ("M-j", sendMessage MirrorExpand),
         ("M-k", sendMessage MirrorShrink),
         ("M-<Tab>", BW.focusDown), ---- With Boring (BW)
         ("M-S-<Tab>", BW.focusUp),
         ("M-g", windows W.focusDown), -- Without BW
         ("M-S-g", windows W.focusUp),
         ("M-m", BW.focusMaster), -- %! Move focus to the master window
         -- modifying the window order
         ("M-S-m", windows W.swapMaster),
         -- Checkout: BW.siftDown, BW.siftUp
         ("M-S-j", BW.swapDown), -- %! Swap the focused window with the next window
         ("M-S-k", BW.swapUp) -- %! Swap the focused window with the previous window
       ]

myTabConfig =
  def
    { activeColor = "#0080FF",
      activeBorderColor = "#0080FF",
      activeTextColor = "#FFFFFF",
      inactiveColor = "#171D23",
      inactiveBorderColor = "#171D23",
      inactiveTextColor = "#9E9E9E",
      urgentColor = "#EC7875",
      urgentBorderColor = "#EC7875",
      urgentTextColor = "#FFFFFF",
      decoHeight = 17,
      fontName = "xft:Iosevka:weight=bold:pixelsize=13:antialias=true"
    }

myLayout =
  showWName $
    avoidStruts $
      BW.boringWindows $ -- Diff b/w BW.boringWindows and BW.boringAuto ??
        onWorkspaces ["8", "9"] (tab ||| tiled) $
          tiled ||| tab
  where
    full = smartBorders $ renamed [Replace "Full"] $ minimize $ Simplest
    mtile = renamed [Replace "MTall"] $ Mirror tiled
    tab = noBorders (tabbed shrinkText myTabConfig)
    tiled =
      smartBorders $
        renamed [Replace "Tall"] $
          reflectHoriz $
            maximizeWithPadding 1 $
              minimize $
                smartSpacing 2 $
                  ResizableTall nmaster delta ratio []
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

myTerminal :: String
myTerminal = "alacritty"

myWorkspaces = map show [1 .. 9 :: Int]

windowCount :: X (Maybe String)
windowCount =
  gets $
    Just
      . show
      . length
      . W.integrate'
      . W.stack
      . W.workspace
      . W.current
      . windowset

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat,
      className =? "mpv" --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)),
      -- className =? "mpv" --> doCenterFloat,
      className =? "Firefox" <&&> resource =? "Toolkit" --> doFloat, -- Firefox pip
      resource =? "desktop_window" --> doIgnore,
      className =? "TelegramDesktop" --> viewShift (myWorkspaces !! 8),
      className =? "Tor Browser" --> viewShift (myWorkspaces !! 8),
      className =? "qBittorrent" --> viewShift (myWorkspaces !! 8),
      className =? "calibre" --> viewShift (myWorkspaces !! 8),
      className =? "Liferea" --> viewShift (myWorkspaces !! 7),
      className =? "Thunderbird" --> viewShift (myWorkspaces !! 7),
      isDialog --> doFloat,
      isFullscreen --> doFullFloat,
      checkDock --> doLower
    ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "bash $HOME/.config/startup/xmonad_startup"
  spawnOnce "bash $HOME/.local/bin/polybar-xmonad.sh"
  spawnOnce "sxhkd -c $HOME/.config/sxhkd/xmonad_sxhkdrc"
