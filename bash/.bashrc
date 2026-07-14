# ~/.bashrc

# ─── PATH CONFIG ───────────────────────────────────────────────────────────────

export PATH="$HOME/.local/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:$PATH"

export PATH="$PATH:$HOME/.roswell/bin"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.qlot/bin:$PATH"
export PATH="$HOME/.nimble/bin:$PATH"

# Go
export PATH="$PATH:/usr/local/go/bin:$HOME/go/bin"

# Codon
export PATH="$PATH:$HOME/.codon/bin"

# ─── ENVIRONMENT ───────────────────────────────────────────────────────────────

export EDITOR=emacs
export XCURSOR_THEME=Bibata-Modern-Ice
export XCURSOR_SIZE=24
export QML_XHR_ALLOW_FILE_READ=1

# ─── SMART TERM DETECTION ──────────────────────────────────────────────────────

set_term_if_supported() {
    infocmp "$1" >/dev/null 2>&1 && export TERM="$1"
}

if [[ "$TERM_PROGRAM" == "Kitty" ]] || [[ "$TERM" == *kitty* ]]; then
    set_term_if_supported xterm-kitty
elif [[ -n "$FOOT" ]] || [[ "$TERM" == "foot" ]]; then
    set_term_if_supported foot
elif [[ "$TERM_PROGRAM" == "WezTerm" ]] || [[ "$TERM" == "wezterm" ]]; then
    set_term_if_supported wezterm
elif [[ -n "$TMUX" ]]; then
    set_term_if_supported screen-256color
elif infocmp xterm-256color >/dev/null 2>&1; then
    export TERM=xterm-256color
elif infocmp xterm >/dev/null 2>&1; then
    export TERM=xterm
else
    export TERM=dumb
fi

# ─── GIT PROMPT ────────────────────────────────────────────────────────────────

git_prompt_info() {
    local branch dirty

    branch=$(git symbolic-ref --quiet --short HEAD 2>/dev/null || \
             git rev-parse --short HEAD 2>/dev/null) || return

    if ! git diff --quiet --ignore-submodules --exit-code 2>/dev/null || \
       ! git diff --cached --quiet --ignore-submodules --exit-code 2>/dev/null; then
        dirty=" *"
    fi

    printf " %s%s" "$branch" "$dirty"
}

PS1='\[\e[36;1m\]\W\[\e[0m\] \[\e[33m\]$(git_prompt_info)\[\e[0m\] \[\e[33;1m\]\$\[\e[0m\] '

# ─── ALIASES ───────────────────────────────────────────────────────────────────

alias pacman='sudo pacman'
alias ssh='TERM=xterm-256color ssh'
alias hx='helix'
alias vi='nvim'
alias vim='nvim'
alias e='emacs -nw'
alias rsbcl='rlwrap sbcl'
alias bigloo='rlwrap bigloo'
alias slem='lem-sdl2'
alias bat='bat --style=full --theme=ansi'
alias man="man -P 'bat -l man -p'"
alias csi='chicken-csi'
alias csc='chicken-csc'
alias cp='cp -rf'
alias git-tree='git log --graph --oneline --decorate --all'

if command -v lsd >/dev/null 2>&1; then
    alias ls='lsd --group-dirs=first --icon=always'
    export LS_MODE=lsd
else
    alias ls='ls --color=auto -h'
    export LS_MODE=gnu-ls
fi

alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias ip='ip --color=auto'

# ─── HISTORY ───────────────────────────────────────────────────────────────────

HISTFILE="$HOME/.bash_history"
HISTSIZE=10000
HISTFILESIZE=10000

shopt -s histappend
PROMPT_COMMAND='history -a; history -n'

HISTCONTROL=ignoredups:erasedups

# ─── READLINE ──────────────────────────────────────────────────────────────────

set -o emacs

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

bind '"\e[1;5D": backward-word'
bind '"\e[1;5C": forward-word'

# ─── BASH COMPLETION ────────────────────────────────────────────────────────────

if [[ -f /usr/share/bash-completion/bash_completion ]]; then
    source /usr/share/bash-completion/bash_completion
fi

# ─── ZOXIDE ─────────────────────────────────────────────────────────────────────

eval "$(zoxide init bash)"
