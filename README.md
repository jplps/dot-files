# Dot Files

Files and/or directories with a . prepended to their name are considered "dot-files". The leading dot . is used as an indicator by software like bash and nautilus to not list these files normally but only when they are specifically requested like pressing Ctrl+H in explorers for example. 

Dot-files are used to store configurations for different applications but they are sometimes used otherwise. 

People tend to backup & also share their dot-files so others can boot-strap their own applications using those configuration files. Please refer to https://dotfiles.github.io/ and build your own!

## Bash

When the shell is executed, there are initialization files it reads which help to setup an environment for itself, and the system user, that are predefined - and customized - functions, variables, aliases and so on.

One can store aliases that will be executed in the .bash_aliases, and, in this case, a .profile file because there isn't a ~/.bash_profile or ~/.bash_login.

### .bash_aliases

Global bash (terminal, or cli - command line interface) aliases files.

### .profile

Contains git parsers to gather data from the branches, and a prompt-export to show things differently.

Note: ~/.bash_profile is only sourced by bash when started in interactive login mode (when you login at the console, or connecting via ssh).

When you log in graphically, ~/.profile will be specifically sourced by the script that launches the graphic session (the desktop env). So ~/.bash_profile is not sourced at all.

When you open a terminal, it starts bash in non-login interactive mode, which means it will source ~/.bashrc. This effect with .profile should be apparent next time you log in.

## Git
### .gitconfig

Basic global configurations for the git program. Contains few variable definitions. See https://git-scm.com/docs/git-config.

## Cron
### .jobs

The directory .jobs has a special task. It keeps the bash scripts that will be runned by cron. Cron is a time-based job scheduler in Unix-like computer operating systems, execute 'man cron' in your terminal for documentation.

Let's say we want pm2 (https://pm2.keymetrics.io/) to start our npm project whenever the server system reboots. That's a simple command executed by bash that goes into reboot.sh (pm2 start npm -- start).

In your home folder:

	$ sudo systemctl status cron.service (see if the daemon is running)
	$ mkdir .jobs && cd .jobs/
	$ touch reboot.sh
	$ crontab -e
		\*This opens the cron file and you will have to insert the following rule to cron knows what script to execute on reboot:
		@reboot /home/admin/.jobs/reboot.sh
	$ update-rc.d cron defaults

@LPS