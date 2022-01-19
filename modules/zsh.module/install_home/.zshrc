HISTFILE=~/.history.zsh
HISTSIZE=1000
SAVEHIST=1000
unsetopt autocd beep
setopt auto_pushd
bindkey -e

# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle ':compinstall' filename '/home/jwolf/.zshrc'
zstyle ':completion:*' menu select

autoload -Uz compinit
compinit

autoload -Uz promptinit
promptinit
prompt redhat

### config paths
export XDG_CONFIG_HOME="$HOME/.config"
export ZSH_CONFIG_HOME="$XDG_CONFIG_HOME/zsh"

### path
export WORKSPACE_DIR="$HOME/Workspace"
export PATH="$PATH:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/.npm/bin:$HOME/.emacs.d/bin"

### modular config
zshrc_modules="$(find "$ZSH_CONFIG_HOME/zsh.d/" -type f -iname '*.zsh')"
for module in "${zshrc_modules[@]}"; do
  source "$module"
done

### custom aliases
[[ ! -f "$ZSH_CONFIG_HOME/aliases" ]] || source "$ZSH_CONFIG_HOME/aliases"

### local config
[[ ! -f "$ZSH_CONFIG_HOME/local" ]] || source "$ZSH_CONFIG_HOME/local"
