#!/bin/bash

user=$(whoami)
home_folder=/home/$user

function check_success {
	$1 &> /dev/null
	if [ $? -eq 0 ]; then
		# echo "Success, exit status: $?"
		echo $2
	else
		echo "Failure, exit status: $?"
	fi
}

check_success 'sudo apt-get upgrade' '- Upgraded'
check_success 'sudo apt-get update' '- Updated'
check_success 'sudo apt-get install -f' '- Extra Packages'
check_success 'sudo apt-get autoremove' '- Autoremove'
check_success 'sudo apt-get clean' '- Clean'
check_success 'sudo apt-get autoclean' '- Autoclean'