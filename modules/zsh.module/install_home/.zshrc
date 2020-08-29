export XDG_CONFIG_HOME="$HOME/.config"
export ZSH_CONFIG_HOME="$XDG_CONFIG_HOME/zsh"

### antigen
source "$ZSH_CONFIG_HOME/antigen"
antigen use oh-my-zsh
antigen bundle hlissner/zsh-autopair
antigen bundle jocelynmallon/zshmarks
antigen theme romkatv/powerlevel10k
antigen apply

### custom opts
unsetopt share_history

### powerlevel10k
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
[[ ! -f "$ZSH_CONFIG_HOME/p10k" ]] || source "$ZSH_CONFIG_HOME/p10k"

### environment
export WORKSPACE_DIR="$HOME/Workspace"
export PATH="$PATH:$HOME/.local/bin:$HOME/.cargo/bin"

if [[ -z "${EDITOR}" ]] ; then
  export ALTERNATE_EDITOR="${HOME}/.local/bin/emacsq"
  export EDITOR='emacsclient'
fi
if [[ -n "${EDITOR}" && -z "${VISUAL}" ]] ; then
  export VISUAL="${EDITOR}"
fi

### java
hash javac 2>/dev/null && {
  export JAVA_HOME="$(dirname $(dirname $(readlink $(readlink $(which javac)))))"
  export ANT_HOME="/usr/share/ant"
  export PATH="$PATH:$JAVA_HOME/bin"
}

### custom aliases
[[ ! -f "$ZSH_CONFIG_HOME/aliases" ]] || source "$ZSH_CONFIG_HOME/aliases"
