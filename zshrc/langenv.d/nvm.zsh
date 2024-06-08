# Unalias hacky nvm alias once before activating nvm, or it might stuck loading.
unalias nvm > /dev/null 2>&1
unsetopt mark_dirs

export NVM_DIR="$HOME/.nvm"
if [ -s "$NVM_DIR/nvm.sh" ]; then
    . "$NVM_DIR/nvm.sh"  # This loads nvm
elif which brew > /dev/null; then
    # if brew found, try to use nvm installed via brew.
    [ -s "$(brew --prefix nvm)/nvm.sh" ] &&
        . "$(brew --prefix nvm)/nvm.sh"
fi

. "$(brew --prefix nvm)/nvm.sh"

setopt mark_dirs
# Fix double-slash in PATH causing keep unloading activated runtime.
alias nvm='() { unsetopt mark_dirs; nvm $@; setopt mark_dirs; }'
