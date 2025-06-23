# ~/.bashrc

### PATH ###
# Function to remove duplicates in PATH
remove_path_duplicates() {
  local OLD_PATH="$1"
  local NEW_PATH=""
  local IFS=':'
  local DIR
  declare -A SEEN

  for DIR in $OLD_PATH; do
    if [[ -n "$DIR" && -z "${SEEN[$DIR]}" ]]; then
      NEW_PATH+="$DIR:"
      SEEN[$DIR]=1
    fi
  done

  # Remove trailing colon
  NEW_PATH="${NEW_PATH%:}"

  echo "$NEW_PATH"
}

# Build desired path order explicitly
DESIRED_PATH="$HOME/.local/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin"

# Append existing PATH to include any others
FULL_PATH="$DESIRED_PATH:$PATH"

# Remove duplicates while preserving order
export PATH=$(remove_path_duplicates "$FULL_PATH")


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


### ALIASES ###

# Always alias pacman to use sudo
alias pacman='sudo pacman'

#Force TERM to xterm-256color
alias ssh='TERM=xterm-256color ssh'

# Conditionally alias 'ls' based on available tools
#if command -v lsd >/dev/null 2>&1; then
#  # If lsd is installed, use it with preferred options
#  alias ls='lsd --group-dirs=first --icon=always'
#  export LS_MODE="lsd"
#else
#  # Fallback to regular ls with color and -h for sizes
#  alias ls='ls --color=auto -h'
#  export LS_MODE="gnu-ls"
#fi

# Neat Colorized Grep, Diff, etc.
alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias ip='ip --color=auto'
