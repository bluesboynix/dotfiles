# ─── BASH GUARD ────────────────────────────────────────────────────────────
# Ensure we are running in Bash
[ -z "$BASH_VERSION" ] && return

# ─── PATH CONFIG ────────────────────────────────────────────────────────────
# Collapse all PATH exports into a single, deduplicated PATH
prepend_path() {
    case ":$PATH:" in
        *":$1:"*) ;;
        *) PATH="$1:$PATH" ;;
    esac
}

# Start with system defaults
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin"

prepend_path "$HOME/.local/bin"
prepend_path "$HOME/.roswell/bin"
prepend_path "$HOME/.cargo/bin"
prepend_path "$HOME/.qlot/bin"
prepend_path "$HOME/.nimble/bin"
prepend_path "/usr/local/go/bin"
prepend_path "$HOME/go/bin"
prepend_path "$HOME/.codon/bin"

# If GOPATH is set, add its bin as well
[ -n "$GOPATH" ] && prepend_path "$GOPATH/bin"

# ─── OTHER VARIABLES ────────────────────────────────────────────────────────
export EDITOR=emacs
export XCURSOR_THEME=Bibata-Modern-Ice
export XCURSOR_SIZE=24
export QML_XHR_ALLOW_FILE_READ=1

# ─── SMART TERM DETECTION ───────────────────────────────────────────────────
set_term_if_supported() {
    local term=$1
    if command -v infocmp >/dev/null 2>&1 && infocmp "$term" >/dev/null 2>&1; then
        export TERM="$term"
        return 0
    fi
    return 1
}

if [[ "$TERM_PROGRAM" == "Kitty" ]] || [[ "$TERM" == *"kitty"* ]]; then
    set_term_if_supported "xterm-kitty"
elif [[ -n "$FOOT" ]] || [[ "$TERM" == "foot" ]]; then
    set_term_if_supported "foot"
elif [[ "$TERM_PROGRAM" == "WezTerm" ]] || [[ "$TERM" == "wezterm" ]]; then
    set_term_if_supported "wezterm"
elif [[ -n "$TMUX" ]]; then
    set_term_if_supported "screen-256color"
elif set_term_if_supported "xterm-256color"; then
    :
elif set_term_if_supported "xterm"; then
    :
else
    export TERM="dumb"
fi

# ─── GIT PROMPT INFO (COMPATIBLE WITH BASH) ─────────────────────────────────
git_prompt_info() {
    local branch dirty
    branch=$(git symbolic-ref --quiet --short HEAD 2>/dev/null || git rev-parse --short HEAD 2>/dev/null)
    [[ -z "$branch" ]] && return

    if ! git diff --quiet --ignore-submodules --exit-code 2>/dev/null ||
       ! git diff --cached --quiet --ignore-submodules --exit-code 2>/dev/null; then
        dirty="*"   # red colour will be added in PS1
    fi

    echo " $branch$dirty"
}

# ─── PROMPT SETUP ───────────────────────────────────────────────────────────
# Use PROMPT_COMMAND to dynamically build PS1
PROMPT_COMMAND=__build_prompt
__build_prompt() {
    local git_info="$(git_prompt_info)"
    # Add colour to the dirty asterisk
    if [[ "$git_info" == *"*" ]]; then
        git_info="${git_info/\*/\[\e[31m\]*\[\e[0m\]}"
    fi
    # Cyan bold current directory, yellow git info, yellow bold $
    PS1='\[\e[36;1m\]\W\[\e[0m\]\[\e[33m\]'"$git_info"'\[\e[0m\] \[\e[33;1m\]\$\[\e[0m\] '
}

# ─── ALIASES ────────────────────────────────────────────────────────────────
alias pacman='sudo pacman'
alias ssh='TERM=xterm-256color ssh'
alias hx="helix"
alias vi="nvim"
alias vim="nvim"
alias e="emacs -nw"
alias rsbcl='rlwrap sbcl'
alias bigloo="rlwrap bigloo"
alias slem="lem-sdl2"
alias bat="bat --style=full --theme=ansi"
alias man="man -P 'bat -l man -p'"
alias csi="chicken-csi"
alias csc="chicken-csc"
alias cp="cp -rf"

# Smart ls alias
if command -v lsd >/dev/null 2>&1; then
    alias ls='lsd --group-dirs=first --icon=always'
else
    alias ls='ls --color=auto -h'
fi

# Colorized tools
alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias ip='ip --color=auto'

# git
alias git-tree="git log --graph --oneline --decorate --all"

# ─── SHELL OPTIONS (Bash equivalents of Zsh setopt) ────────────────────────
# Disable beep
bind 'set bell-style none'
# Enable auto cd (type folder name to cd)
shopt -s autocd
# History settings
shopt -s histappend
HISTFILE="$HOME/.bash_history"
HISTSIZE=10000
HISTFILESIZE=10000
HISTCONTROL=ignoreboth:erasedups   # ignorespace + ignoredups, remove older duplicates
# Share history across sessions instantly
PROMPT_COMMAND="history -a; history -c; history -r; ${PROMPT_COMMAND}"

# ─── COMPLETION ─────────────────────────────────────────────────────────────
# Load bash completion (often needs bash-completion package)
if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
# Menu-like completion: show all possibilities on double Tab
bind 'set show-all-if-ambiguous on'
bind 'set menu-complete-display-prefix on'

# ─── HISTORY SEARCH (Up/Down on current input) ─────────────────────────────
# Use readline's history-search-backward/forward (requires inputrc or bind)
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
# For terminals that send \eOA / \eOB as well
bind '"\eOA": history-search-backward'
bind '"\eOB": history-search-forward'

# ─── KEYBINDINGS (terminfo‑aware) ───────────────────────────────────────────
# Helper to bind terminfo keys
bind_term_key() {
    local cap="$1" func="$2"
    local seq
    seq=$(tput "$cap" 2>/dev/null) || return
    [ -n "$seq" ] && bind "\"$seq\": $func"
}

bind_term_key khome     beginning-of-line
bind_term_key kend      end-of-line
bind_term_key kich1     overwrite-mode
bind_term_key kbs       backward-delete-char
bind_term_key kdch1     delete-char
bind_term_key kcuu1     history-search-backward   # Up
bind_term_key kcud1     history-search-forward    # Down
bind_term_key kcub1     backward-char
bind_term_key kcuf1     forward-char
bind_term_key kpp       beginning-of-buffer-or-history
bind_term_key knp       end-of-buffer-or-history
bind_term_key kcbt      reverse-menu-complete      # Shift-Tab

bind_term_key kLFT5     backward-word             # Ctrl+Left
bind_term_key kRIT5     forward-word              # Ctrl+Right

# Fallback bindings for common escape sequences
bind '"\e[1~": beginning-of-line'
bind '"\e[4~": end-of-line'
bind '"\e[5~": beginning-of-buffer-or-history'
bind '"\e[6~": end-of-buffer-or-history'
bind '"\e[3~": delete-char'
bind '"\e[2~": overwrite-mode'

# ─── APPLICATION MODE (optional, mimics Zsh's zle‑hooks) ───────────────────
# Enable keypad transmit mode so arrow keys send distinct codes
if tput smkx >/dev/null 2>&1; then
    # Send smkx before each prompt, and rmkx after the command finishes
    # This is best effort; Bash lacks clean pre/post hooks.
    # We'll add smkx to the prompt building and rmkx in a DEBUG trap.
    __build_prompt() {
        # Original prompt builder (redefine after we defined it above)
        local git_info="$(git_prompt_info)"
        if [[ "$git_info" == *"*" ]]; then
            git_info="${git_info/\*/\[\e[31m\]*\[\e[0m\]}"
        fi
        # Send smkx (non-printing, so wrap in \[ \])
        tput smkx | sed 's/\x1b/\\e/g'  # not needed; we can just embed the raw sequence
        PS1='\[$(tput smkx)\]\n\[\e[36;1m\]\W\[\e[0m\]\[\e[33m\]'"$git_info"'\[\e[0m\] \[\e[33;1m\]\$\[\e[0m\] '
    }
    # Reset on command execution
    trap 'tput rmkx' DEBUG
fi

# ─── ZOXIDE ─────────────────────────────────────────────────────────────────
if command -v zoxide >/dev/null 2>&1; then
    eval "$(zoxide init bash)"
fi

# ─── (REMOVED) ZSH PLUGINS ──────────────────────────────────────────────────
# The following Zsh‑only features are not available in Bash:
#   - zinit plugin manager
#   - zsh-syntax-highlighting
#   - zsh-autosuggestions
#   - zsh-completions (use bash-completion package instead)
#   - compinit / zstyle completion styling
# Consider using `ble.sh` for syntax highlighting & autosuggestions in Bash.
