#==============================================================================
# ~/.bashrc
# Modern Bash configuration
#==============================================================================

# Only continue for interactive Bash shells
[[ $- != *i* ]] && return
[[ -z $BASH_VERSION ]] && return

#------------------------------------------------------------------------------
# Environment
#------------------------------------------------------------------------------

export EDITOR="emacs"
export XCURSOR_THEME="Bibata-Modern-Ice"
export XCURSOR_SIZE=24
export QML_XHR_ALLOW_FILE_READ=1

#------------------------------------------------------------------------------
# PATH
#------------------------------------------------------------------------------

# Start from a clean system PATH
export PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin"

prepend_path() {
    local dir=$1

    [[ -d "$dir" ]] || return

    case ":$PATH:" in
        *":$dir:"*) ;;
        *) PATH="$dir:$PATH" ;;
    esac
}

for dir in \
    "$HOME/.local/bin" \
    "$HOME/.cargo/bin" \
    "$HOME/.roswell/bin" \
    "$HOME/.qlot/bin" \
    "$HOME/.nimble/bin" \
    "$HOME/.codon/bin" \
    "/usr/local/go/bin" \
    "$HOME/go/bin"
do
    prepend_path "$dir"
done

[[ -n "$GOPATH" ]] && prepend_path "$GOPATH/bin"

export PATH

unset -f prepend_path

#------------------------------------------------------------------------------
# Terminal detection
#------------------------------------------------------------------------------

set_term() {
    local candidate

    for candidate in "$@"; do
        if command -v infocmp >/dev/null 2>&1 &&
           infocmp "$candidate" >/dev/null 2>&1
        then
            export TERM="$candidate"
            return 0
        fi
    done

    return 1
}

case "$TERM_PROGRAM:$TERM" in
    Kitty:*|*:xterm-kitty*)
        set_term xterm-kitty
        ;;

    WezTerm:*|*:wezterm*)
        set_term wezterm
        ;;

    *:foot*)
        set_term foot
        ;;

    *)
        if [[ -n "$TMUX" ]]; then
            set_term screen-256color
        else
            set_term xterm-256color xterm
        fi
        ;;
esac

: "${TERM:=dumb}"

unset -f set_term

#------------------------------------------------------------------------------
# Shell Options
#------------------------------------------------------------------------------

shopt -s \
    autocd \
    histappend \
    checkwinsize

bind 'set bell-style none'

#------------------------------------------------------------------------------
# History
#------------------------------------------------------------------------------

HISTFILE="$HOME/.bash_history"

HISTSIZE=10000
HISTFILESIZE=10000

HISTCONTROL=ignoreboth:erasedups
HISTIGNORE="ls:bg:fg:history:exit"

history_sync() {
    history -a
    history -c
    history -r
}

#------------------------------------------------------------------------------
# Git Prompt Helper
#------------------------------------------------------------------------------

git_prompt_info() {

    git rev-parse --is-inside-work-tree >/dev/null 2>&1 || return

    local branch
    local dirty=""

    branch=$(
        git symbolic-ref --quiet --short HEAD 2>/dev/null ||
        git rev-parse --short HEAD 2>/dev/null
    ) || return

    git diff --quiet --ignore-submodules 2>/dev/null ||
        dirty="*"

    git diff --cached --quiet --ignore-submodules 2>/dev/null ||
        dirty="*"

    printf "%s%s" "$branch" "$dirty"
}

#==============================================================================
# Prompt
#==============================================================================

build_prompt() {
    local git

    git=$(git_prompt_info)

    if [[ -n "$git" ]]; then
        if [[ "$git" == *"*" ]]; then
            git="${git%\*}\[\e[31m\]*\[\e[33m\]"
        fi    PS1="\[\e[36;1m\]\W\[\e[0m\]${git} \[\e[33;1m\]\\$\[\e[0m\] "
    git_info="$(git_prompt_info)"

    PS1="${CLR_CYAN}\W${CLR_RESET}"

    if [[ -n "$git_info" ]]; then
        if [[ "$git_info" == *"*" ]]; then
            git_info="${git_info%\*}${CLR_RED}*${CLR_YELLOW}"
        fi

        PS1+=" ${CLR_YELLOW}${git_info}${CLR_RESET}"
    fi

    PS1+=" ${CLR_BOLD_YELLOW}\\$${CLR_RESET} "
}
-----------------------------------------------------------------------------
# Prompt Command
#------------------------------------------------------------------------------

prompt_command() {
    history_sync
    build_prompt
}

PROMPT_COMMAND=(prompt_command)

#==============================================================================
# Aliases
#==============================================================================

#
# Editors
#

alias e='emacs -nw'
alias hx='helix'
alias vi='nvim'
alias vim='nvim'

#
# System
#

alias pacman='sudo pacman'
alias cp='cp -rf'

#
# SSH
#

alias ssh='TERM=xterm-256color ssh'

#
# Git
#

alias git-tree='git log --graph --oneline --decorate --all'

#
# Lisp
#

alias rsbcl='rlwrap sbcl'
alias bigloo='rlwrap bigloo'
alias slem='lem-sdl2'

#
# Chicken Scheme
#

alias csi='chicken-csi'
alias csc='chicken-csc'

#
# Better tools
#

alias bat='bat --style=full --theme=ansi'
alias man="man -P 'bat -l man -p'"

#
# ls
#

if command -v lsd >/dev/null 2>&1; then
    alias ls='lsd --group-dirs=first --icon=always'
else
    alias ls='ls --color=auto -h'
fi

#
# Colored utilities
#

alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias ip='ip --color=auto'

#==============================================================================
# Convenience
#==============================================================================

# Make parent directories automatically
alias mkdir='mkdir -pv'

# Human-readable disk usage
alias df='df -h'

# Human-readable memory usage
alias free='free -h'

# Safer remove/move
alias mv='mv -i'
alias rm='rm -i'

# Faster clear
alias cls='clear'

#==============================================================================
# Bash Completion
#==============================================================================

for file in \
    /usr/share/bash-completion/bash_completion \
    /etc/bash_completion
do
    [[ -r "$file" ]] && source "$file" && break
done

#==============================================================================
# Readline
#==============================================================================

# Disable terminal bell
bind 'set bell-style none'

# Show all matches immediately on double-Tab
bind 'set show-all-if-ambiguous on'
bind 'set menu-complete-display-prefix on'

#==============================================================================
# History Search
#==============================================================================

# Press Up/Down to search history matching the current command prefix.

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

# Alternate escape sequences (some terminals)
bind '"\eOA": history-search-backward'
bind '"\eOB": history-search-forward'

#==============================================================================
# Key Bindings
#==============================================================================

bind_term_key() {
    local cap=$1
    local func=$2
    local seq

    seq=$(tput "$cap" 2>/dev/null) || return

    [[ -n "$seq" ]] && bind "\"$seq\": $func"
}

# Navigation

bind_term_key khome beginning-of-line
bind_term_key kend  end-of-line

bind_term_key kcub1 backward-char
bind_term_key kcuf1 forward-char

bind_term_key kcuu1 history-search-backward
bind_term_key kcud1 history-search-forward

# Editing

bind_term_key kdch1 delete-char
bind_term_key kbs backward-delete-char
bind_term_key kich1 overwrite-mode

# Buffer navigation

bind_term_key kpp beginning-of-buffer-or-history
bind_term_key knp end-of-buffer-or-history

# Shift-Tab

bind_term_key kcbt reverse-menu-complete

# Ctrl + Left / Right

bind_term_key kLFT5 backward-word
bind_term_key kRIT5 forward-word

#------------------------------------------------------------------------------
# Common fallback sequences
#------------------------------------------------------------------------------

bind '"\e[1~": beginning-of-line'
bind '"\e[4~": end-of-line'

bind '"\e[2~": overwrite-mode'
bind '"\e[3~": delete-char'

bind '"\e[5~": beginning-of-buffer-or-history'
bind '"\e[6~": end-of-buffer-or-history'

#==============================================================================
# Optional Programs
#==============================================================================

#
# zoxide
#

if command -v zoxide >/dev/null 2>&1; then
    eval "$(zoxide init bash)"
fi

#==============================================================================
# Environment Tweaks
#==============================================================================

# Better less behavior
export LESS='-FRi'

# Preserve colors through less
export LESS_TERMCAP_mb=$'\e[1;31m'
export LESS_TERMCAP_md=$'\e[1;36m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[1;44;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;32m'

#==============================================================================
# Cleanup
#==============================================================================

unset file
unset -f bind_term_keyB
