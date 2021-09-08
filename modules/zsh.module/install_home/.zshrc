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
setopt auto_pushd
setopt complete_in_word
setopt rm_star_silent
setopt hist_find_no_dups
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_verify

### powerlevel10k
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
[[ ! -f "$ZSH_CONFIG_HOME/p10k" ]] || source "$ZSH_CONFIG_HOME/p10k"

### environment
export WORKSPACE_DIR="$HOME/Workspace"
export PATH="$PATH:$HOME/.local/bin:$HOME/.cargo/bin"

### custom aliases
[[ ! -f "$ZSH_CONFIG_HOME/aliases" ]] || source "$ZSH_CONFIG_HOME/aliases"

### modular config
zshrc_modules="$(find "$ZSH_CONFIG_HOME/zsh.d/" -type f -iname '*.zsh')"
for module in "${zshrc_modules[@]}"; do
  source "$module"
done

### local config
[[ ! -f "$ZSH_CONFIG_HOME/local" ]] || source "$ZSH_CONFIG_HOME/local"
