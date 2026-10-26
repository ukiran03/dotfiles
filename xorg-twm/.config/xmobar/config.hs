import System.Environment (getArgs)
import Xmobar

data Theme = Theme
  { fg :: String
  , bg :: String
  , tBorder :: String
  , gray :: String
  , red :: String
  , green :: String
  , blue :: String
  , cyan :: String
  , icon :: String
  }

scriptDir :: String
scriptDir = "/home/ukiran/.config/xmobar/scripts/"

chargeScript :: String
chargeScript = scriptDir ++ "charge-notify.sh"

trayerScript :: String
trayerScript = scriptDir ++ "trayer-padding-icon.sh"

lightTheme :: Theme
lightTheme =
  Theme
    { fg = "#000000"
    , bg = "#f2f2f2"
    , tBorder = "#e0e0e0"
    , gray = "#6D8895"
    , red = "#a60000"
    , green = "#006800"
    , blue = "#0031a9"
    , cyan = "#3f578f"
    , icon = "xmonad_light.xpm"
    }

darkTheme :: Theme
darkTheme =
  Theme
    { fg = "#ffffff"
    , bg = "#0d0e1c"
    , tBorder = "#3c4449"
    , gray = "#6D8895"
    , red = "#ff5f59"
    , green = "#88ca9f"
    , blue = "#42A5F5"
    , cyan = "#00eff0"
    , icon = "xmonad_dark.xpm"
    }

-- Make the config
makeConfig :: Theme -> Config
makeConfig t =
  defaultConfig
    { overrideRedirect = False
    , font = "xft:Iosevka Nerd Font:size=13.25:bold:antialias=true;3"
    , additionalFonts =
        [ "xft:Iosevka Nerd Font:pixelsize=17:bold:antialias=true"
        , "xft:FontAwesome:style=Regular:pixelsize=14"
        ]
    , border = BottomB
    , lowerOnStart = True
    , fgColor = fg t
    , bgColor = bg t
    , borderColor = tBorder t
    , iconRoot = "/home/ukiran/.config/xmobar/xpm/"
    , commands =
        [ Run
            (BatteryP
               ["BAT1"]
               [ "-t"
               , " <fc=" ++ gray t ++ ">b</fc><acstatus><left> "
               , "-L"
               , "20"
               , "-H"
               , "85"
               , "-l"
               , red t
               , "-h"
               , green t
               , "--"
               , "-O"
               , "<fc=" ++ green t ++ "><fn=1>\xF1E6 </fn></fc>"
               , "-o"
               , "<fc=" ++ red t ++ ">- </fc>"
               , "-a"
               , chargeScript
               , "-P"
               , "%"
               , "-A"
               , "20"
               ]
               100)
        , Run
            (Com "sh" ["-c", "brightnessctl -m | cut -d, -f4"] "brightness" 20)
        , Run
            (Network
               "wlp3s0"
               ["-t", "<fc=" ++ green t ++ "> <dev><up> </fc>"]
               30)
        , Run
            (Com
               "sh"
               [ "-c"
               , "awk '/wlp3s0/ {print $2 + $10}' /proc/net/dev | numfmt --to=iec"
               ]
               "netusage"
               30)
        , Run
            (Cpu
               [ "-t"
               , "<fc=" ++ gray t ++ ">c</fc><total>%"
               , "-L"
               , "3"
               , "-H"
               , "50"
               , "--high"
               , red t
               , "--normal"
               , green t
               ]
               50)
        , Run
            (Memory
               [ "-t"
               , "<fc=" ++ gray t ++ ">m</fc><used><fc=" ++ gray t ++ ">G</fc>"
               , "-d"
               , "1"
               , "--"
               , "--scale"
               , "1024"
               ]
               50)
        , Run
            (DiskU
               [ ("/", "<fc=" ++ gray t ++ ">r</fc><free>")
               , ("/home", "<fc=" ++ gray t ++ ">~</fc><free>")
               ]
               ["-L", "20", "-H", "50", "-m", "1", "-p", "3"]
               50)
        , Run
            (Com
               "sh"
               [ "-c"
               , "datediff 2002-11-20 today -f '<fc="
                   ++ blue t
                   ++ ">%yy %mm %dd</fc>'"
               ]
               "age"
               0)
        , Run
            (Com
               "sh"
               ["-c", "notmuch count 'tag:unread and tag:inbox'"]
               "notmuch"
               6000)
        , Run
            (Date
               ("%a, %d %b %Y, <fc=" ++ cyan t ++ ">%I:%M:%S %p (%R)</fc>")
               "date"
               10)
        , Run (Com trayerScript [] "trayerpad" 50)
        , Run
            (Volume
               "default"
               "Master"
               [ "-t"
               , "<status> <volume>%"
               , "--"
               , "-O"
               , "<fn=1>\xF028 </fn>"
               , "-o"
               , "<fn=1>\xEEE8 </fn>"
               , "-C"
               , blue t
               , "-c"
               , gray t
               , "-H"
               , "60"
               , "-h"
               , "<fc=" ++ red t ++ "><fn=1>\xF071 </fn></fc>"
               ]
               10)
        , Run XMonadLog
        ]
    , sepChar = "%"
    , alignSep = "}{"
    , template =
        " <icon="
          ++ icon t
          ++ "/> <fc="
          ++ tBorder t
          ++ ">|</fc> %XMonadLog% } %date% {<fc="
          ++ green t
          ++ "><fn=1>\x294D</fn></fc>%netusage% %wlp3s0% <fc="
          ++ tBorder t
          ++ ">|</fc> <fc="
          ++ gray t
          ++ ">\xF42F  </fc>%notmuch% <fc="
          ++ gray t
          ++ ">\xF00DF </fc>%brightness%  %disku%  %cpu% %memory%  %default:Master% %battery% %trayerpad%"
    }

main :: IO ()
main = do
  args <- getArgs
  let chosenTheme =
        case args of
          ("light":_) -> lightTheme
          _ -> darkTheme
  xmobar (makeConfig chosenTheme)
