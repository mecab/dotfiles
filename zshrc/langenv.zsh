basedir="$(dirname "${BASH_SOURCE:-$0}")"

for file in ${basedir}/langenv.d/*.zsh; do
    source "$file"
done
