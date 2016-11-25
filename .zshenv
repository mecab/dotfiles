# Do not use path_helper (/etc/profile)
setopt no_global_rcs

# パスの設定
## 重複したパスを登録しない。
typeset -U path
## (N-/): 存在しないディレクトリは登録しない。
##    パス(...): ...という条件にマッチするパスのみ残す。
##            N: NULL_GLOBオプションを設定。
##               globがマッチしなかったり存在しないパスを無視する。
##            -: シンボリックリンク先のパスを評価。
##            /: ディレクトリのみ残す。
path=(
    $HOME/bin(N-/)
    $HOME/local/bin(N-/)
    $HOME/.local/bin(N-/)
    /usr/local/bin(N-/)
    /usr/bin(N-/)
    /bin(N-/)
    # MacPorts
    /opt/local/bin(N-/)
    # TeXLive2016
    /usr/local/texlive/2016/bin/universal-darwin(N-/)
)

# sudo時のパスの設定
## -x: export SUDO_PATHも一緒に行う。
## -T: SUDO_PATHとsudo_pathを連動する。
typeset -xT SUDO_PATH sudo_path
## 重複したパスを登録しない。
typeset -U sudo_path
## (N-/): 存在しないディレクトリは登録しない。
##    パス(...): ...という条件にマッチするパスのみ残す。
##            N: NULL_GLOBオプションを設定。
##               globがマッチしなかったり存在しないパスを無視する。
##            -: シンボリックリンク先のパスを評価。
##            /: ディレクトリのみ残す。
sudo_path=({,/usr/pkg,/usr/local,/usr}/sbin(N-/))

# man用パスの設定
## 重複したパスを登録しない。
typeset -U manpath
## (N-/) 存在しないディレクトリは登録しない。
##    パス(...): ...という条件にマッチするパスのみ残す。
##            N: NULL_GLOBオプションを設定。
##               globがマッチしなかったり存在しないパスを無視する。
##            -: シンボリックリンク先のパスを評価。
##            /: ディレクトリのみ残す。
manpath=(
    $HOME/local/share/man(N-/)
    # MacPorts
    /opt/local/share/man(N-/)
    # システム用
    /usr/local/share/man(N-/)
    /usr/share/man(N-/))

export EDITOR="emacsclient -cnw"
export PAGER=less

# lvの設定
## -c: ANSIエスケープシーケンスの色付けなどを有効にする。
## -l: 1行が長くと折り返されていても1行として扱う。
##     （コピーしたときに余計な改行を入れない。）
export LV="-c -l"

if [ "$PAGER" != "lv" ]; then
    ## lvがなくてもlvでページャーを起動する。
    alias lv="$PAGER"
fi

# lessの設定
## -R: ANSIエスケープシーケンスのみ素通しする。
export LESS="-R"
export LESSCHARSET=utf-8

# grepの設定
## GNU grepがあったら優先して使う。
if type ggrep > /dev/null 2>&1; then
    alias grep=ggrep
fi

# GLS優先 + カラー
case $(uname) in
    *BSD|Darwin)
        alias ls="ls -G"
        if [ `which gls` ]; then
            alias ls="gls --color"
        fi
        ;;
    *)
        alias ls="ls --color"
        ;;
esac

export LANG=ja_JP.UTF-8
export LC_CTYPE=ja_JP.UTF-8
# export LC_LANG=en.US_UTF-8
