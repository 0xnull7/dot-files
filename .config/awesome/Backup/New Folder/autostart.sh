#!/usr/bin/env bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

run "nm-applet"
run "picom"
run "~/.config/awesome/My_Screen_Layout.sh"

#run "megasync"
#run "xscreensaver -no-splash"
#run "/usr/bin/dropbox"
#run "insync start"
#run "/usr/bin/redshift"
#run "mpd"
