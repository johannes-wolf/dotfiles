# Emacs aliases
alias e="emacs-wrapper"
alias m='e --eval "(progn (magit-status) (delete-other-windows))"'

# Force terminal mode
alias mt="m -t"
alias et="e -t"

# Env
export EDITOR="emacs-wrapper"
export VISUAL="${EDITOR}"
