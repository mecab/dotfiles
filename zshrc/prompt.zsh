autoload -Uz colors
colors

GREEN="%{$fg[green]%}"
LGREEN="%{$fg_bold[green]%}"
RED="%{$fg_bold[red]%}"
LRED="%{$fg_bold[red]%}"
RESET="%{$reset_color%}"
WHITE="%{$fg[white]%}"
LWHITE="%{$fg_bold[white]%}"
USERCOLOR="%(!.${LRED}.${LGREEN})"
EMOJI_ANGRY=$'\U1F4A2'
EMOJI_MONEYBAG=$'\U1F4B0'

# prompt_1="${LGREEN}%n@%m [%~]${RESET} %(1j,(%j,)"
prompt_1="${USERCOLOR}[%n@`hostcolor`%m${RESET} ${WHITE}%~${USERCOLOR}] ${RESET}${WHITE} %(1j,(%j,)${RESET}"

### 2行目左にでるプロンプト。
###   %h: ヒストリ数。
###   %(1j,(%j),): 実行中のジョブ数が1つ以上ある場合だけ「(%j)」を表示。
###     %j: 実行中のジョブ数。
###   %{%B%}...%{%b%}: 「...」を太字にする。
###   %#: 一般ユーザなら「%」、rootユーザなら「#」になる。
prompt_2="[%h]%{%B%}%(!.%2{${EMOJI_ANGRY}%} .%2{${EMOJI_MONEYBAG}%} )%{%b%}"
PROMPT='${prompt_1}
${prompt_2}'

## Show branch name in Zsh's right prompt

autoload -Uz VCS_INFO_get_data_git; VCS_INFO_get_data_git 2> /dev/null
setopt prompt_subst
setopt re_match_pcre

function rprompt-git-current-branch {
    local name st color gitdir action
    if [[ "$PWD" =~ '/\.git(/.*)?$' ]]; then
        return
    fi

    name=`git rev-parse --abbrev-ref=loose HEAD 2> /dev/null`

    if [[ -z $name ]]; then
        return
    fi

    gitdir=`git rev-parse --git-dir 2> /dev/null`
    action=`VCS_INFO_git_getaction "$gitdir"` && action="($action)"

    st=`git status 2> /dev/null`
    if [[ "$st" =~ "(?m)^nothing to" ]]; then
        color=%F{blue}
    elif [[ "$st" =~ "(?m)^nothing added" ]]; then
        color=%F{yellow}
    elif [[ "$st" =~ "(?m)^# Untracked" ]]; then
        color=%B%F{magenta}
    else
        color=%F{magenta}
    fi

    res="$color$name$action%f%b"
    if [ -n "$res" ]; then
        echo "[$res]"
    else
        echo ""
    fi
}

RPROMPT='`rprompt-git-current-branch`'
