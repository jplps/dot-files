# Autoloading
autoload -Uz compinit && compinit

# Nvm paths
export NVM_DIR="$HOME/.nvm"
  [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"
  [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"

# Base PATH
export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Export docker envs
export DOCKER_HOST=unix:///$HOME/.colima/default/docker.sock
export TESTCONTAINERS_RYUK_DISABLED=true

# Export tty to GPG
export GPG_TTY=$(tty)

# Git prompt
source ~/.git-prompt.sh
setopt PROMPT_SUBST ; PS1='%F{green}%n%f %F{yellow}%~%f%F{red}$(__git_ps1 " %s")%f $ '
# precmd () { __git_ps1 "%n" " %~ $ " " %s" }

[ -f "/Users/joao.silva/.ghcup/env" ] && source "/Users/joao.silva/.ghcup/env" # ghcup-env

function update-bp-projects() {
    if [ -d "$HOME/Projects/bp" ]; then
        cd "$HOME/Projects/bp" || return 1
        if [ -f "$HOME/.git-update.sh" ]; then
            sh .git-update.sh
        else
            echo -e "\n.git-update.sh script not found."
        fi
    else
        echo -e "\n$HOME/Projects/bp directory not found."
    fi
}
