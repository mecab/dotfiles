# If runnning on Bash on Windows, suppress the error related to nice(5)
# See: https://github.com/Microsoft/BashOnWindows/issues/1887
if [[ $(uname -r) =~ Microsoft$ ]]; then
    unsetopt BG_NICE
fi

if [ -z $SUDO_COMMAND ] && [ "$NO_AUTOSTART_TMUX" -ne 1 ]; then
    # if not in sudo, run tmuxx.
    test -z "$TMUX" && tmuxx
fi

source ~/.ls_colors.sh
source ~/.dotfiles/lib/color.sh

source ~/.dotfiles/lib/zsh-history-substring-search/zsh-history-substring-search.zsh
source ~/.dotfiles/lib/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.dotfiles/lib/fzf-tab/fzf-tab.plugin.zsh
source ~/.dotfiles/lib/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.dotfiles/lib/manai/manai.zsh
source <(fzf --zsh)

# fzf は常に fzf-tmux を使う（fzf は他のエイリアスからも使われるのでエイリアスではなく関数として定義）
fzf () {
    if [ -n "$TMUX" ]; then
        fzf-tmux $@
    else
        command fzf $@
    fi
}

# Unset vars to prevent them from being appended to multiple times if bash
# shells are nested and as a result .bashrc is sourced multiple times
unset FZF_ALT_C_OPTS FZF_CTRL_R_OPTS FZF_DEFAULT_OPTS

# View full path in preview window (?)
export FZF_ALT_C_OPTS="${FZF_ALT_C_OPTS:+$FZF_ALT_C_OPTS }--preview 'echo {}' --preview-window down:5:hidden:wrap --bind '?:toggle-preview'"

# View full command in preview window (?)
export FZF_CTRL_R_OPTS="${FZF_CTRL_R_OPTS:+$FZF_CTRL_R_OPTS }--preview 'echo {}' --preview-window down:5:hidden:wrap --bind '?:toggle-preview'"

# ウィンドウタイトル
## 実行中のコマンドとユーザ名とホスト名とカレントディレクトリを表示。
update_title_pre() {
    local command_line=
    typeset -a command_line
    command_line=${(z)2}
    local command=
    if [ ${(t)command_line} = "array-local" ]; then
        command="$command_line[1]"
    else
        command="$2"
    fi

    if [[ ${#command} -gt 32 ]]; then
        command="${command[1,32]}..."
    fi

    print -n -P "\e]2;"
    echo -n "🌀 ${command} ("
    print -rnD "$PWD)"
    print -n -P "\a"
}

update_title_post() {
    last_status="$?"
    command="$history[$((HISTCMD-1))]"
    if [[ ${#command} -gt 32 ]]; then
        command="${command[1,32]}..."
    fi
    print -n -P "\e]2;"
    if [[ "$last_status" == 0 ]]; then
        echo -n "✅ "
    else
        echo -n "✖  "
    fi
    echo -n "${command} ("
    print -rnD "$PWD)"
    print -n -P "\a"
}
## X環境上でだけウィンドウタイトルを変える。
# if [ -n "$DISPLAY" -a -z "$INSIDE_EMACS" ]; then
    preexec_functions=($preexec_functions update_title_pre)
    precmd_functions=($precmd_functions update_title_post)
# fi

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
bindkey '\eh' manai

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

setopt auto_param_slash
setopt auto_menu
setopt auto_param_keys

# 補完
fpath=(~/.dotfiles/zsh_completion $fpath)
autoload -U compinit && compinit -i

## 補完方法毎にグループ化する。
### 補完方法の表示方法
###   %B...%b: 「...」を太字にする。
###   %d: 補完方法のラベル
# zstyle ':completion:*' format '%B%d%b'
# zstyle ':completion:*' group-name ''

# set descriptions format to enable group support
# NOTE: don't use escape sequences here, fzf-tab will ignore them
zstyle ':completion:*:descriptions' format '[%d]'
# set list-colors to enable filename colorizing
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# force zsh not to show completion menu, which allows fzf-tab to capture the unambiguous prefix
zstyle ':completion:*' menu no
# switch group using `<` and `>`
zstyle ':fzf-tab:*' switch-group '<' '>'

## 補完侯補をメニューから選択する。
### select=2: 補完候補を一覧から選択する。
###           ただし、補完候補が2つ以上なければすぐに補完する。
# zstyle ':completion:*:default' menu select=2

## 補完候補に色を付ける。
### "": 空文字列はデフォルト値を使うという意味。
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
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

setopt complete_aliases

### Start emacs as a daemon if it is not started.
if ! pgrep -i emacs >/dev/null 2>&1; then
    echo Starting Emacs daemon in background ...
    ### `\emacs --daemon 2> /dev/null &`
    \emacs --daemon 2> /dev/null &
fi

dzf() {
    service=$(docker-compose config --services | fzf)
    docker-compose $(echo $@ | sed -E "s/@/${service}/")
}

if [ -f $HOME/google-cloud-sdk/completion.zsh.inc ]; then
    source $HOME/google-cloud-sdk/completion.zsh.inc
fi

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh" || true

source ~/.dotfiles/alias.zsh
for file in ~/.dotfiles/zshrc/*.zsh; do
    source "$file"
done
if [ -f "$HOME/.zshrc.local.inc" ]; then
    source "$HOME/.zshrc.local.inc"
fi

# Enable job controll (just in case it is disabled accidentally)
setopt monitor

# auto compilation of .zshrc
if [ ! -f ~/.zshrc.zwc -o ~/.zshrc -nt ~/.zshrc.zwc ]; then
    echo "Compiling .zshrc..." &
    zcompile ~/.zshrc
fi
