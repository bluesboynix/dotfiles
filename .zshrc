export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="robbyrussell"

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

export PATH=$PATH:~/.roswell/bin

export EDITOR=nvim


### Smart TERM setup for SSH/local/tmux/modern terminals ###
# Priority: kitty > foot > wezterm > tmux > 256color > xterm > fallback

# Function to safely set TERM if it's supported
set_term_if_supported() {
  local term=$1
  if infocmp "$term" >/dev/null 2>&1; then
    export TERM="$term"
    return 0
  fi
  return 1
}

# 1. Detect Kitty
if [[ "$TERM_PROGRAM" == "Kitty" ]] || [[ "$TERM" == *"kitty"* ]]; then
  set_term_if_supported "xterm-kitty"
# 2. Detect foot (based on env var)
elif [[ -n "$FOOT" ]] || [[ "$TERM" == "foot" ]]; then
  set_term_if_supported "foot"
# 3. Detect wezterm (based on env var)
elif [[ "$TERM_PROGRAM" == "WezTerm" ]] || [[ "$TERM" == "wezterm" ]]; then
  set_term_if_supported "wezterm"
# 4. Inside tmux
elif [[ -n "$TMUX" ]]; then
  set_term_if_supported "screen-256color"
# 5. Normal 256color xterm
elif set_term_if_supported "xterm-256color"; then
  :
# 6. Fallback to basic xterm
elif set_term_if_supported "xterm"; then
  :
# 7. Very last resort fallback
else
  export TERM="dumb"
fi


### PLUGINS ###
plugins=(
  git
  colored-man-pages
  colorize
)

source $ZSH/oh-my-zsh.sh


### ALIASES ###
# Always alias pacman to use sudo
alias pacman='sudo pacman'

# Force TERM to xterm-256color
alias ssh='TERM=xterm-256color ssh'

# Alias neovim/nvim to vi
alias vi="nvim"
alias vim="nvim"

# Conditionally alias 'ls' based on available tools
if command -v lsd >/dev/null 2>&1; then
  # If lsd is installed, use it with preferred options
  alias ls='lsd --group-dirs=first --icon=always'
  export LS_MODE="lsd"
else
  # Fallback to regular ls with color and -h for sizes
  alias ls='ls --color=auto -h'
  export LS_MODE="gnu-ls"
fi

# Neat Colorized Grep, Diff, etc.
alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias ip='ip --color=auto'
alias sbcl='rlwrap sbcl'
alias slem="lem-sdl2"