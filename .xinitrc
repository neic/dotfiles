# Option    "RegistryDwords"    "EnableBrightnessControl=1;PowerMizerEnable=0x1;PerfLevelSrc=0x3333;PowerMizerLevel=0x3;PowerMizerDefault=0x3;PowerMizerDefaultAC=0x3"

xrdb -merge ~/.Xresources
# xmonad & wmpid=$!
awesome & wmpid=$!
emacs --daemon &
dropboxd &
skype &
sleep 10
xmodmap ~/.Xmodmap &
wait $wmpid
