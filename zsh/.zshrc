# Plugins manager zinit 
# Load Zinit
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
[ ! -d $ZINIT_HOME ] && mkdir -p "$(dirname $ZINIT_HOME)"
[ ! -d $ZINIT_HOME/.git ] && git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
source "${ZINIT_HOME}/zinit.zsh"

# Plugins
zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions

# Completion styling
zstyle ':completion:*' mathcer-list 'm:{a-z}={A-Za-z}'

# ─── PATH CONFIG ────────────────────────────────────────────────────────────────

typeset -U path
path=(
  $HOME/.local/bin
  /bin
  /usr/bin
  /usr/local/bin
  /sbin
  /usr/sbin
)
export PATH="${(j/:/)path}"
export PATH="$PATH:$HOME/.roswell/bin"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/home/bluesboy/.qlot/bin:$PATH"
export PATH="/home/bluesboy/.nimble/bin:$PATH"

# ─── other varialbes ─────────────────────────────────────────────────────

export EDITOR=emacs
export XCURSOR_THEME=Bibata-Modern-Ice
export XCURSOR_SIZE=24

# ─── SMART TERM DETECTION ───────────────────────────────────────────────────────

set_term_if_supported() {
  local term=$1
  if infocmp "$term" >/dev/null 2>&1; then
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

# ─── GIT PROMPT INFO (MINIMAL AGNOSTER STYLE) ───────────────────────────────────

autoload -Uz vcs_info

setopt prompt_subst  # <-- This is crucial

git_prompt_info() {
  local branch dirty

  branch=$(git symbolic-ref --quiet --short HEAD 2>/dev/null || git rev-parse --short HEAD 2>/dev/null)
  [[ -z "$branch" ]] && return

  if ! git diff --quiet --ignore-submodules --exit-code 2>/dev/null ||
     ! git diff --cached --quiet --ignore-submodules --exit-code 2>/dev/null; then
    dirty="%F{red} *%f"
  fi

  echo " $branch$dirty"
}

# ─── PROMPT SETUP ───────────────────────────────────────────────────────────────
PROMPT='%F{cyan}%B%1~%b%f %F{yellow}$(git_prompt_info)%f %F{yellow}%B$%b%f '

# ─── ALIASES ────────────────────────────────────────────────────────────────────

alias pacman='sudo pacman'
alias ssh='TERM=xterm-256color ssh'
alias hx="helix"
alias vi="nvim"
alias vim="nvim"
alias e="emacs"
alias rsbcl='rlwrap sbcl'
alias bigloo="rlwrap bigloo"
alias slem="lem-sdl2"
alias bat="bat --style=full --theme=ansi"
alias man="man -P 'bat -l man -p'"
alias csi="chicken-csi"
alias csc="chicken-csc"

# Smart ls alias
if command -v lsd >/dev/null 2>&1; then
  alias ls='lsd --group-dirs=first --icon=always'
  export LS_MODE="lsd"
else
  alias ls='ls --color=auto -h'
  export LS_MODE="gnu-ls"
fi

# Colorized tools
alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias ip='ip --color=auto'

# git
alias git-tree="git log --graph --oneline --decorate --all"

# Command completion
autoload -Uz compinit
compinit
zstyle ':completion:*' menu select


# Optional: disable beeping
setopt NO_BEEP
# Enable auto cd (type folder name to cd into it)
setopt AUTO_CD


# History search
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

setopt appendhistory     # Save each command to history file immediately
setopt sharehistory          # Share history across all sessions
setopt hist_ignore_dups       # Ignore duplicate entries
setopt hist_ignore_all_dups   # Remove older duplicates
setopt hist_find_no_dups      # Don't show duplicates during history search
setopt hist_save_no_dups     # Don't save duplicates in history file
setopt hist_reduce_blanks    # Remove extra blanks in commands
setopt hist_ignore_dups
# ─── KEYBINDINGS: ZKBD + FALLBACKS ──────────────────────────────────────────────

# Create zkbd-compatible hash
typeset -g -A key

# Terminal-aware (terminfo-based) key mappings
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

# Bind terminfo keys if defined
[[ -n "${key[Home]}"      ]] && bindkey -- "${key[Home]}"       beginning-of-line
[[ -n "${key[End]}"       ]] && bindkey -- "${key[End]}"        end-of-line
[[ -n "${key[Insert]}"    ]] && bindkey -- "${key[Insert]}"     overwrite-mode
[[ -n "${key[Backspace]}" ]] && bindkey -- "${key[Backspace]}"  backward-delete-char
[[ -n "${key[Delete]}"    ]] && bindkey -- "${key[Delete]}"     delete-char
[[ -n "${key[Up]}"        ]] && bindkey -- "${key[Up]}"         up-line-or-history
[[ -n "${key[Down]}"      ]] && bindkey -- "${key[Down]}"       down-line-or-history
[[ -n "${key[Left]}"      ]] && bindkey -- "${key[Left]}"       backward-char
[[ -n "${key[Right]}"     ]] && bindkey -- "${key[Right]}"      forward-char
[[ -n "${key[PageUp]}"    ]] && bindkey -- "${key[PageUp]}"     beginning-of-buffer-or-history
[[ -n "${key[PageDown]}"  ]] && bindkey -- "${key[PageDown]}"   end-of-buffer-or-history
[[ -n "${key[Shift-Tab]}" ]] && bindkey -- "${key[Shift-Tab]}"  reverse-menu-complete


key[Control-Left]="${terminfo[kLFT5]}"
key[Control-Right]="${terminfo[kRIT5]}"

[[ -n "${key[Control-Left]}"  ]] && bindkey -- "${key[Control-Left]}"  backward-word
[[ -n "${key[Control-Right]}" ]] && bindkey -- "${key[Control-Right]}" forward-word

# ─── ENTER APPLICATION MODE FOR TERMINAL ───────────────────────────────────────

# Tell terminal to switch to application mode (for correct key codes)
if (( ${+terminfo[smkx]} && ${+terminfo[rmkx]} )); then
  autoload -Uz add-zle-hook-widget
  function zle_application_mode_start { echoti smkx }
  function zle_application_mode_stop  { echoti rmkx  }
  add-zle-hook-widget -Uz zle-line-init    zle_application_mode_start
  add-zle-hook-widget -Uz zle-line-finish  zle_application_mode_stop
fi

# zoxide
eval "$(zoxide init zsh)"
