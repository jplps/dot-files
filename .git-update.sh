#!/bin/bash

# Start ssh-agent and add your SSH key
if ! eval "$(ssh-agent)"; then
    echo "Error: Failed to start ssh-agent."
    exit 1
fi

if ! ssh-add /Users/joao.silva/.ssh/id_ed25519; then
    echo "Error: Failed to add SSH key."
    exit 1
fi

echo    # Add line break

check_shell() {
    if [[ -n "$BASH_VERSION" ]]; then
        echo -e "$(tput setaf 7)$1$(tput sgr0)"
    elif [[ -n "$ZSH_VERSION" ]]; then
        echo -e "%F{white}$1%f"
    else
        echo "Updating $1"
    fi
}

export -f check_shell

update_repo() {
    if [ -d "$1/.git" ]; then
        cd "$1" || exit
        echo -e "$(check_shell "Updating $1")"
        if git pull --all 2>&1 | sed "s/^/$(check_shell)/"; then
            # Purge local branches that have been deleted on the remote
            echo -e "$(check_shell "Purging local branches...")"
            git fetch --prune origin
            git branch -vv | grep ': gone]' | awk '{print $1}' | xargs -r git branch -D

            # Purge remote tracking branches that no longer exist on the remote
            echo -e "$(check_shell "Purging remote tracking branches...")"
            git remote prune origin
        else
            echo "Error: Failed to update repository."
        fi
    else
        echo "Error: Not a git repository."
    fi
    echo
}

export -f update_repo

find . -maxdepth 1 -type d -not -path "." -exec bash -c 'update_repo "$0"' {} \;

# Stop ssh-agent when the script is done
if ! eval "$(ssh-agent -k)"; then
    echo "Error: Failed to stop ssh-agent."
fi
