for file in "$(dirname "${BASH_SOURCE:-$0}")"/langenv.d/*.zsh; do
    source "$file"
done
