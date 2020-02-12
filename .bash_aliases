# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# ALIASES
# flip wacom to left handed freaks:
alias left='xsetwacom set "Wacom Bamboo 16FG 4x5 Pen stylus" rotate half'

# scan the wifi list using nmcli (debian-xfce):
alias wscan='nmcli dev wifi list'

# clear sesh forall (debian 10/2020):
alias cleanh='history -c && sudo rm -r ~/.bash_history'

# i am shutting this too much down
alias std='sudo shutdown now'
alias rbt='sudo reboot'

# sys related
alias las='ls -al'
alias sag='sudo apt-get'
alias imin='sag update && sag upgrade'
alias fresh='sag install -f && sag autoremove'
alias unlock='sudo rm /var/lib/dpkg/lock'