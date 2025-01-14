#!/bin/sh

for i in `seq 1 10`; do
	screen -ls 'xmonad-bg' && break
	sleep 1
done

# trayer
pidof trayer || screen -S "xmonad-bg" -X screen \
	trayer \
		--edge bottom --align right --SetDockType true \
		--SetPartialStrut false --expand true --widthtype request \
		--height 24  --transparent true --tint 0x000000 --monitor 0

# nm-applet
pidof nm-applet || screen -S "xmonad-bg" -X screen \
	nm-applet

# Firefox
pidof firefox || screen -S "xmonad-bg" -X screen \
	firefox -P "default" -no-remote
