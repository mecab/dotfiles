## ページャーを使いやすくする。
### grep -r def *.rb L -> grep -r def *.rb |& lv
alias -g L="|& ${PAGER}"
## grepを使いやすくする。
alias -g G='| grep'
## 後はおまけ。
alias -g H='| head'
alias -g T='| tail'
alias -g S='| sed'

## 完全に削除。
alias rr="command rm -rf"
## ファイル操作を確認する。
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

## pushd/popdのショートカット。
alias pd="pushd"
alias po="popd"

alias la="ls -a"
alias lf="ls -F"
alias ll="ls -all"

alias du="du -h"
alias df="df -h"

alias su="su -l"

alias bess='(){ bat --color=always "$@" | less }'
alias bexx='(){ bat --color=always "$@" | less -X }'

alias -s py=python

alias ssh="TERM=xterm-256color ssh"

alias emacs="emacsclient -nw"
alias e="emacsclient -nw"
alias kille="emacsclient -e '(kill-emacs)'"

alias -g B='$(git branch -a --no-color --format="%(refname:short)" | fzf --height=50% --header=branches)'

if type kubectl >/dev/null 2>&1; then
    alias kg='kubectl get'
    alias kgp='kubectl get pods -o wide'
    alias kgpw='watch kubectl get pods -o wide'
    alias ka='kubectl apply'
    alias kd='kubectl describe'
    alias kdel='kubectl delete'
    alias kc='kubectl config current-context'
    alias kcu='kubectl config use-context'
    alias kcc='kubectl config current-context'
    alias kp='kubectl proxy'
    alias kl='kubectl logs'
    alias ke='kubectl exec -it'
    alias kgan='kubectl get all --all-namespaces'
    alias kga='kubectl get all'
    alias kgi='kubectl get pods -o jsonpath="{..containers..image}" | tr -s "[[:space:]]" "\n" | sort | uniq'
    alias ktx='(){ if [[ -n "$1" ]]; then kubectl config use-context "$1"; else kubectl config use-context $(kubectl config get-contexts -o name | fzf --height=50%); fi }'
    alias kns='(){ if [[ -n "$1" ]]; then kubectl config set-context --current --namespace="$1"; else kubectl config set-context --current --namespace=$(kubectl get namespaces --no-headers -o custom-columns=':metadata.name' | fzf --height=50%); fi }'
    alias kezf='(){ kubectl exec -it $(kubectl get pods -oname | sed -E "s/pod\///g" | fzf) $@ }'

    alias -g K='$(kubectl get pods -oname | sed -E "s/pod\///g" | fzf --height=50% --header=pods)'
fi

### utils
alias randomstr='(){ cat /dev/urandom | LC_CTYPE=C tr -dc "0-9a-zA-Z" | fold -w $1 | head -n 1 }'
alias colorize_rbl="colorecho -w -p '/- debug: /,gray' -p '/- info: /,cyan' -p '/- error: /,red' -p '/\[hostName=.*?\]/,blue' -p '/\[serviceName=.*?\]/,green' -p '/\[correlationId=.*?\]/,yellow' -p '/\[clientId=.*?\]/,magenta'"
alias branch_from_issue="git checkout -b \$(jira issue list -q'project IS NOT EMPTY and status IN (OPEN, \"IN PROGRESS\")' -a \`jira me\` --plain --no-headers --columns key,summary | fzf --height=50% | perl -pE 's/[ \t]/-/g;' -pE 's/[^a-zA-Z0-9 _-]//g;' -pE 's/[-]+/-/g;' -pE 's/^(.+?)-([0-9]+)-(.+)/\$1-\$2-\L\$3/g');"
alias hl='(){ egrep --color "$|.*$1.*" }'

### github copilot
type gh >/dev/null 2>&1 && eval "$(gh copilot alias -- zsh)"
