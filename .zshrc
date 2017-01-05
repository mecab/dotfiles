source ~/.ls_colors.sh
source ~/.dotfiles/etc/lib/color.sh
source ~/.zplug/init.zsh

# zplug "hchbaw/auto-fu.zsh", at:pu
zplug "stedolan/jq", from:gh-r, as:command, rename-to:jq, \
      on: "b4b4r07/emoji-cli"
zplug "zsh-users/zsh-syntax-highlighting", defer:3
zplug "junegunn/fzf-bin", as:command, from:gh-r, rename-to:fzf
# ついでに tmux 用の拡張も入れるといい
zplug "junegunn/fzf", as:command, use:bin/fzf-tmux
zplug "b4b4r07/enhancd", use:init.sh
zplug "mrowa44/emojify", as:command
zplug "zsh-users/zsh-history-substring-search", hook-build:"__zsh_version 4.3"

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi
zplug load

# ウィンドウタイトル
## 実行中のコマンドとユーザ名とホスト名とカレントディレクトリを表示。
update_title() {
    local command_line=
    typeset -a command_line
    command_line=${(z)2}
    local command=
    if [ ${(t)command_line} = "array-local" ]; then
        command="$command_line[1]"
    else
        command="$2"
    fi
    print -n -P "\e]2;"
    echo -n "(${command})"
    print -n -P " %n@%m:%~\a"
}
## X環境上でだけウィンドウタイトルを変える。
if [ -n "$DISPLAY" -a -z "$INSIDE_EMACS" ]; then
    preexec_functions=($preexec_functions update_title)
fi

## Emacsキーバインドを使う。
bindkey -e
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
# bindkey "^p" history-beginning-search-backward-end
# bindkey "^n" history-beginning-search-forward-end
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
# bindkey "\\ep" history-beginning-search-backward-end
# bindkey "\\en" history-beginning-search-forward-end

setopt print_eight_bit

# ディレクトリ移動
## ディレクトリ名だけでcdする。
setopt auto_cd
## cdで移動してもpushdと同じようにディレクトリスタックに追加する。
setopt auto_pushd
## カレントディレクトリ中に指定されたディレクトリが見つからなかった場合に
## 移動先を検索するリスト。
cdpath=(~)
## ディレクトリが変わったらディレクトリスタックを表示。
chpwd_functions=($chpwd_functions dirs)

# ヒストリ
## ヒストリを保存するファイル
HISTFILE=~/.zsh_history
## メモリ上のヒストリ数。
## 大きな数を指定してすべてのヒストリを保存するようにしている。
HISTSIZE=10000000
## 保存するヒストリ数
SAVEHIST=$HISTSIZE
## ヒストリファイルにコマンドラインだけではなく実行時刻と実行時間も保存する。
setopt extended_history
## 同じコマンドラインを連続で実行した場合はヒストリに登録しない。
setopt hist_ignore_dups
## スペースで始まるコマンドラインはヒストリに追加しない。
setopt hist_ignore_space
## すぐにヒストリファイルに追記する。
setopt inc_append_history
## zshプロセス間でヒストリを共有する。
setopt share_history
## C-sでのヒストリ検索が潰されてしまうため、出力停止・開始用にC-s/C-qを使わない。
setopt no_flow_control

# プロンプト
## PROMPT内で変数展開・コマンド置換・算術演算を実行する。
setopt prompt_subst
## PROMPT内で「%」文字から始まる置換機能を有効にする。
setopt prompt_percent
## コピペしやすいようにコマンド実行後は右プロンプトを消す。
setopt transient_rprompt

autoload -Uz colors
colors

setopt auto_param_slash
setopt auto_menu
setopt auto_param_keys

GREEN="%{$fg[green]%}"
LGREEN="%{$fg_bold[green]%}"
RED="%{$fg_bold[red]%}"
LRED="%{$fg_bold[red]%}"
RESET="%{$reset_color%}"
WHITE="%{$fg[white]%}"
LWHITE="%{$fg_bold[white]%}"
USERCOLOR="%(!.${LRED}.${LGREEN})"

# prompt_1="${LGREEN}%n@%m [%~]${RESET} %(1j,(%j,)"
prompt_1="${USERCOLOR}[%n@`hostcolor`%m${RESET} ${WHITE}%~${USERCOLOR}] ${RESET}${WHITE} %(1j,(%j,)${RESET}"

### 2行目左にでるプロンプト。
###   %h: ヒストリ数。
###   %(1j,(%j),): 実行中のジョブ数が1つ以上ある場合だけ「(%j)」を表示。
###     %j: 実行中のジョブ数。
###   %{%B%}...%{%b%}: 「...」を太字にする。
###   %#: 一般ユーザなら「%」、rootユーザなら「#」になる。
prompt_2="[%h]%{%B%}%(!.💢  .💰  )%{%b%"
PROMPT='${prompt_1}
${prompt_2} '

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
        color=$LGREEN
    elif [[ "$st" =~ "(?m)^nothing added" ]]; then
        color=%F{yellow}
    elif [[ "$st" =~ "(?m)^# Untracked" ]]; then
        color=%B%F{red}
    else
        color=%F{red}
    fi

    res="$color$name$action%f%b"
    if [ -n "$res" ]; then
        echo "[$res]"
    else
        echo ""
    fi
}

RPROMPT='`rprompt-git-current-branch`'

# 補完

## compinit is prepared by zplug
# autoload -U compinit
# -compinit

## 補完方法毎にグループ化する。
### 補完方法の表示方法
###   %B...%b: 「...」を太字にする。
###   %d: 補完方法のラベル
zstyle ':completion:*' format '%B%d%b'
zstyle ':completion:*' group-name ''

## 補完侯補をメニューから選択する。
### select=2: 補完候補を一覧から選択する。
###           ただし、補完候補が2つ以上なければすぐに補完する。
zstyle ':completion:*:default' menu select=2

## 補完候補に色を付ける。
### "": 空文字列はデフォルト値を使うという意味。
zstyle ':completion:*:default' list-colors ""

## 補完候補がなければより曖昧に候補を探す。
### m:{a-z}={A-Z}: 小文字を大文字に変えたものでも補完する。
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

## 補完方法の設定。指定した順番に実行する。
### _oldlist 前回の補完結果を再利用する。
### _complete: 補完する。
### _match: globを展開しないで候補の一覧から補完する。
### _ignored: 補完候補にださないと指定したものも補完候補とする。
### _approximate: 似ている補完候補も補完候補とする。
### _prefix: カーソル以降を無視してカーソル位置までで補完する。
zstyle ':completion:*' completer \
       _oldlist _complete _match _ignored _approximate _prefix

## 補完候補をキャッシュする。
zstyle ':completion:*' use-cache yes
## 詳細な情報を使う。
zstyle ':completion:*' verbose yes
## sudo時にはsudo用のパスも使う。
zstyle ':completion:sudo:*' environ PATH="$SUDO_PATH:$PATH"

## カーソル位置で補完する。
setopt complete_in_word
## globを展開しないで候補の一覧から補完する。
setopt glob_complete
## 補完時にヒストリを自動的に展開する。
setopt hist_expand
## 補完候補がないときなどにビープ音を鳴らさない。
setopt no_beep
## 辞書順ではなく数字順に並べる。
setopt numeric_glob_sort

# 展開
## --prefix=~/localというように「=」の後でも
## 「~」や「=コマンド」などのファイル名展開を行う。
setopt magic_equal_subst
## 拡張globを有効にする。
## glob中で「(#...)」という書式で指定する。
setopt extended_glob
## globでパスを生成したときに、パスがディレクトリだったら最後に「/」をつける。
setopt mark_dirs

# ジョブ
## jobsでプロセスIDも出力する。
setopt long_list_jobs

# 実行時間
## 実行したプロセスの消費時間が3秒以上かかったら
## 自動的に消費時間の統計情報を表示する。
REPORTTIME=3

# ログイン・ログアウト
## 全てのユーザのログイン・ログアウトを監視する。
watch="all"
## ログイン時にはすぐに表示する。
log

## ^Dでログアウトしないようにする。
setopt ignore_eof

# 単語
## 「/」も単語区切りとみなす。
WORDCHARS=${WORDCHARS:s,/,,}
## 「|」も単語区切りとみなす。
## 2011-09-19
WORDCHARS="${WORDCHARS}|"

# alias

setopt complete_aliases

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

if [ -f /usr/local/bin/virtualenvwrapper_lazy.sh ]; then
    source /usr/local/bin/virtualenvwrapper_lazy.sh
elif [ -f /usr/local/share/python/virtualenvwrapper_lazy.sh ]; then
    source /usr/local/share/python/virtualenvwrapper_lazy.sh
fi

alias -s py=python

if which brew > /dev/null; then
    # for nvm installed thru brew
    export NVM_DIR=$(brew --prefix nvm)/nvm.sh
else
    export NVM_DIR="$HOME/.nvm"
fi
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm


### Start emacs as a daemon if it is not started.
if ! pgrep -i emacs >/dev/null 2>&1; then
    echo Starting Emacs daemon in background ...
    ### `\emacs --daemon 2> /dev/null &`
    \emacs --daemon 2> /dev/null &
fi

alias emacs="emacsclient -cnw"
alias e='emacs'
alias kille="emacsclient -e '(kill-emacs)'"
###

if [ -z $SUDO_COMMAND ]; then
    # if not in sudo, run tmuxx.
    test -z "$TMUX" && $HOME/.dotfiles/bin/tmuxx
fi

# auto compilation of .zshrc
if [ ! -f ~/.zshrc.zwc -o ~/.zshrc -nt ~/.zshrc.zwc ]; then
    echo "Compiling .zshrc..."
    zcompile ~/.zshrc &
fi
