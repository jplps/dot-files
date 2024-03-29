# Dot Files

Check simple "dot-files" that helps us in everyday jobs.

## Concept

Files and/or directories with a "." prepended to their name are considered "dot-files". The leading dot is used as an indicator by software like bash and nautilus to not list these files normally, but only when they are specifically requested like pressing Ctrl+H in explorers for example. They are hidden by default.

Dot-files are used to store configurations for different applications but they are sometimes used otherwise. 

People tend to backup & also share their dot-files so others can boot-strap their own applications using those configuration files. Please refer to https://dotfiles.github.io/ and build your own!

## Bash

When the shell is executed, there are initialization files it reads which help to setup an environment for itself, and the system user, that are predefined - and customized - functions, variables, aliases and so on.

One can store aliases that will be executed in the .bash_aliases, and, in this case, a .profile file.

### .bashrc

Contains git parsers to gather data from the branches, and a prompt-export to show things differently, added by the end of the original (debian 10) .bashrc file.

### .bash_aliases

Global bash (terminal, or cli - command line interface) aliases files called from the .bashrc file.

### Bash Scripting

Scripts run by bash have the .sh extention and can be built to execute something like a backup (home folder copy):

```
# Shell interpreter definition:
#!/bin/bash

# Command execution (simple backup):
tar -czf /tmp/home-directory.tar.gz /home/backup-directory

# Variables and backup:
user=$(whoami)
input=/home/$user
output=/tmp/${user}_home_$(date +%Y-%m-%d_%H%M%S).tar.gz

tar -czf $output $input 2> /dev/null

echo "Backup of $input completed! Output backup file details:"
ls -l $output
```

With output redirection, we can eliminate unwanted stderr messages by redirecting it with 2> notation to /dev/null. Consider /dev/null as a data sink, which discards any data redirected to it ($ man null).


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

## Emacs
### .emacs

This file is an initialization script written in lisp for emacs configurations and packages. This should be in home dir, and emacs will find it and run before initializes.

It contains the setup for dealing with developments in clojure (with cider), org files, plantuml and other interesting things.

See the manual and other stuff at https://www.gnu.org/software/emacs/ for more details.