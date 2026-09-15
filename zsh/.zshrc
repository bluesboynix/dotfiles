# =============================================================================
# ~/.zshrc
# =============================================================================
# Zsh interactive shell configuration
# =============================================================================

# ZINIT
ZINIT_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/zinit/zinit.git"

if [[ ! -d "$ZINIT_HOME/.git" ]]; then
  mkdir -p "${ZINIT_HOME:h}"
  git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi

source "$ZINIT_HOME/zinit.zsh"

# Plugins
zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions

# COMPLETION
autoload -Uz compinit
compinit

zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

# PATH
typeset -U path

path=(
  "$HOME/.local/bin"
  /usr/local/bin
  /usr/bin
  /bin
  /usr/local/sbin
  /usr/sbin

  "$HOME/.roswell/bin"
  "$HOME/.cargo/bin"
  "$HOME/.qlot/bin"
  "$HOME/.nimble/bin"

  /usr/local/go/bin
  "$HOME/go/bin"

  "$HOME/.codon/bin"
)

export PATH="${(j/:/)path}"

# ENVIRONMENT
export EDITOR=emacs

export XCURSOR_THEME="Bibata-Modern-Ice"
export XCURSOR_SIZE=24

export QML_XHR_ALLOW_FILE_READ=1

# TERMINAL
set_term_if_supported() {
  local term="$1"

  if infocmp "$term" >/dev/null 2>&1; then
    export TERM="$term"
    return 0
  fi

  return 1
}

case "$TERM_PROGRAM" in
  Kitty)
    set_term_if_supported "xterm-kitty"
    ;;

  WezTerm)
    set_term_if_supported "wezterm"
    ;;

  *)
    if [[ "$TERM" == *kitty* ]]; then
      set_term_if_supported "xterm-kitty"

    elif [[ -n "$FOOT" || "$TERM" == foot* ]]; then
      set_term_if_supported "foot"

    elif [[ -n "$TMUX" ]]; then
      set_term_if_supported "screen-256color"

    elif set_term_if_supported "xterm-256color"; then
      :

    elif set_term_if_supported "xterm"; then
      :

    else
      export TERM="dumb"
    fi
    ;;
esac

# SHELL OPTIONS
# Do not beep.
setopt NO_BEEP

# Allow:
#   $ cd ~/Projects
# by simply typing:
#   ~/Projects
setopt AUTO_CD

# HISTORY
HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000
SAVEHIST=10000

setopt APPEND_HISTORY
setopt SHARE_HISTORY

setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_REDUCE_BLANKS

# PROMPT
setopt PROMPT_SUBST

git_prompt_info() {
  local branch dirty=""

  branch=$(
    git symbolic-ref --quiet --short HEAD 2>/dev/null ||
    git rev-parse --short HEAD 2>/dev/null
  )

  [[ -z "$branch" ]] && return

  if ! git diff --quiet --ignore-submodules --exit-code 2>/dev/null ||
     ! git diff --cached --quiet --ignore-submodules --exit-code 2>/dev/null
  then
    dirty=" %F{red}*%f"
  fi

  print -r -- " $branch$dirty"
}

PROMPT='%F{#ff1000}%B%1~%b%f %F{blue}$(git_prompt_info)%f %F{#ffff00}%B$%b%f '

# ALIASES
# Package management
alias pacman='sudo pacman'

# Editors
alias e='emacs -nw'
alias vi='nvim'
alias vim='nvim'
alias hx='helix'

# Full Emacs configuration
# alias em='emacs'

# Minimal/lite Emacs configuration
alias leg="emacs --init-directory=$HOME/.config/lite-emacs"
alias le="emacs -nw --init-directory=$HOME/.config/lite-emacs"

# Lisp / Scheme
alias rsbcl='rlwrap sbcl'
alias bigloo='rlwrap bigloo'
alias guile='rlwrap guile --no-auto-compile'
alias csi='chicken-csi'
alias csc='chicken-csc'
alias slem='lem-sdl2'

# Tools
alias bat='bat --style=full --theme=ansi'
alias man="man -P 'bat -l man -p'"

alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias ip='ip --color=auto'

alias cp='cp -rf'

# Git
alias git-tree='git log --graph --oneline --decorate --all'

# LS
if command -v lsd >/dev/null 2>&1; then
  alias ls='lsd --group-dirs=first --icon=always'

  export LS_MODE="lsd"
  export LS_COLORS='di=1;31:ln=35:ex=32:*.zip=38;5;135:*.tar=38;5;135:fi=0;71'

else
  alias ls='ls --color=auto -h'

  export LS_MODE="gnu-ls"
  export LS_COLORS='di=1;31:ln=35:ex=32:*.zip=31:*.tar=31:fi=38;5;244'
fi

# SSH
alias ssh='TERM=xterm-256color ssh'

# ZLE / KEYBOARD
# History search with Up / Down
autoload -Uz \
  up-line-or-beginning-search \
  down-line-or-beginning-search

zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

# Terminfo key map
typeset -g -A key

key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Insert]="${terminfo[kich1]}"
key[Backspace]="${terminfo[kbs]}"
key[Delete]="${terminfo[kdch1]}"

key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"

key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"

key[Shift-Tab]="${terminfo[kcbt]}"

key[Control-Left]="${terminfo[kLFT5]}"
key[Control-Right]="${terminfo[kRIT5]}"

# Basic navigation
[[ -n "${key[Home]}" ]] &&
  bindkey -- "${key[Home]}" beginning-of-line

[[ -n "${key[End]}" ]] &&
  bindkey -- "${key[End]}" end-of-line

[[ -n "${key[Insert]}" ]] &&
  bindkey -- "${key[Insert]}" overwrite-mode

[[ -n "${key[Backspace]}" ]] &&
  bindkey -- "${key[Backspace]}" backward-delete-char

[[ -n "${key[Delete]}" ]] &&
  bindkey -- "${key[Delete]}" delete-char

[[ -n "${key[Left]}" ]] &&
  bindkey -- "${key[Left]}" backward-char

[[ -n "${key[Right]}" ]] &&
  bindkey -- "${key[Right]}" forward-char

[[ -n "${key[PageUp]}" ]] &&
  bindkey -- "${key[PageUp]}" beginning-of-buffer-or-history

[[ -n "${key[PageDown]}" ]] &&
  bindkey -- "${key[PageDown]}" end-of-buffer-or-history

[[ -n "${key[Shift-Tab]}" ]] &&
  bindkey -- "${key[Shift-Tab]}" reverse-menu-complete

# History navigation
[[ -n "${key[Up]}" ]] &&
  bindkey -- "${key[Up]}" up-line-or-beginning-search

[[ -n "${key[Down]}" ]] &&
  bindkey -- "${key[Down]}" down-line-or-beginning-search

# Ctrl + Left / Right
[[ -n "${key[Control-Left]}" ]] &&
  bindkey -- "${key[Control-Left]}" backward-word

[[ -n "${key[Control-Right]}" ]] &&
  bindkey -- "${key[Control-Right]}" forward-word

# TERMINAL APPLICATION MODE
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then

  autoload -Uz add-zle-hook-widget

  zle_application_mode_start() {
    echoti smkx
  }

  zle_application_mode_stop() {
    echoti rmkx
  }

  add-zle-hook-widget -Uz zle-line-init \
    zle_application_mode_start

  add-zle-hook-widget -Uz zle-line-finish \
    zle_application_mode_stop
fi

# ZOXIDE
if command -v zoxide >/dev/null 2>&1; then
  eval "$(zoxide init zsh)"
fi

# END
