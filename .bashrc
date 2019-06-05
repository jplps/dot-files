# author: LPS4d

# SHELL DEV TOOLS
# use it with caution. grab this code and paste to .bashrc at the end of it, reopen the shell and give it a try.

# some personal aliases
# system stuff
alias sag='sudo apt-get'
alias imin='sag update && sag upgrade'
alias fresh='sag install -f && sag autoremove'
alias unlock='sudo rm -r /var/lib/dpkg/lock'
# flip wacom to left handed freaks:
alias left='xsetwacom set "Wacom Bamboo 16FG 4x5 Pen stylus" rotate half'
# scan the wifi list using nmcli (debian-based):
alias wscan='nmcli dev wifi list'
# clean history
alias cleanh='sudo rm -r ~/.bash_history'

# GIT TOOLS
# get current branch in git repo
function parse_git_branch() {
	BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
	if [ ! "${BRANCH}" == "" ]
	then
		STAT=`parse_git_dirty`
		echo " [${BRANCH}${STAT}]"
	else
		echo ""
	fi
}

# get current status of git repo
function parse_git_dirty {
	status=`git status 2>&1 | tee`
	dirty=`echo -n "${status}" 2> /dev/null | grep "modified:" &> /dev/null; echo "$?"`
	untracked=`echo -n "${status}" 2> /dev/null | grep "Untracked files" &> /dev/null; echo "$?"`
	ahead=`echo -n "${status}" 2> /dev/null | grep "Your branch is ahead of" &> /dev/null; echo "$?"`
	newfile=`echo -n "${status}" 2> /dev/null | grep "new file:" &> /dev/null; echo "$?"`
	renamed=`echo -n "${status}" 2> /dev/null | grep "renamed:" &> /dev/null; echo "$?"`
	deleted=`echo -n "${status}" 2> /dev/null | grep "deleted:" &> /dev/null; echo "$?"`
	bits=''
	if [ "${renamed}" == "0" ]; then
		bits=">${bits}"
	fi
	if [ "${ahead}" == "0" ]; then
		bits="*${bits}"
	fi
	if [ "${newfile}" == "0" ]; then
		bits="+${bits}"
	fi
	if [ "${untracked}" == "0" ]; then
		bits="?${bits}"
	fi
	if [ "${deleted}" == "0" ]; then
		bits="x${bits}"
	fi
	if [ "${dirty}" == "0" ]; then
		bits="!${bits}"
	fi
	if [ ! "${bits}" == "" ]; then
		echo " ${bits}"
	else
		echo ""
	fi
}

# export promp with git functions, and yellow info:
export PS1="${debian_chroot:+($debian_chroot)} ⚓\[\033[01;32m\]\u@\h\[\033[00m\] \[\033[01;34m\]\w\[\033[33m\]\`parse_git_branch\`\[\033[01;00m\] \$ "
