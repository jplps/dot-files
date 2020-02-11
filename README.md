# Dot Files

Files and/or directories with a . prepended to their name are considered "dot-files". The leading dot . is used as an indicator by software like bash and nautilus to not list these files normally but only when they are specifically requested like pressing Ctrl+H in explorers for example. 

Dot-files are used to store configurations for different applications but they are sometimes used otherwise. 

People tend to backup & also share their dot-files so others can boot-strap their own applications using those configuration files. Please refer to https://dotfiles.github.io/ and build your own!

### .bash_aliases

Global bash (terminal) configurations aliases. Contains aliases, git tools and prompt-export.

### .gitconfig

Basic global configurations for the git program. Contains few variable definitions.

### .jobs

The directory .jobs has a special task. It keeps the bash scripts that will be runned by cron (research for cron jobs). Cron is a time-based job scheduler in Unix-like computer operating systems, see man cron for documentation.

Let's say we want pm2 (https://pm2.keymetrics.io/) to start our npm project whenever the server system reboots.

In your home folder:

	sudo systemctl status cron.service (see if the daemon is running)
	mkdir .jobs && cd .jobs/
	touch reboot.sh
	crontab -e
		\*This opens the cron file and you will have to insert the following rule to cron knows what script to execute on reboot:
		@reboot /home/admin/.jobs/reboot.sh
	update-rc.d cron defaults
	

@LPS