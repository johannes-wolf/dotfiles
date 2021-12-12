export XDG_CONFIG_HOME="$HOME/.config"
export ZSH_CONFIG_HOME="$XDG_CONFIG_HOME/zsh"

### prompt
eval "$(starship init zsh)"

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

### environment
export WORKSPACE_DIR="$HOME/Workspace"
export PATH="$PATH:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/.npm/bin:$HOME/.emacs.d/bin"

### custom aliases
[[ ! -f "$ZSH_CONFIG_HOME/aliases" ]] || source "$ZSH_CONFIG_HOME/aliases"

### modular config
zshrc_modules="$(find "$ZSH_CONFIG_HOME/zsh.d/" -type f -iname '*.zsh')"
for module in "${zshrc_modules[@]}"; do
  source "$module"
done

### local config
[[ ! -f "$ZSH_CONFIG_HOME/local" ]] || source "$ZSH_CONFIG_HOME/local"
