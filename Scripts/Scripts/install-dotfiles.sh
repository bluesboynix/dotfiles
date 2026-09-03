#!/usr/bin/env bash

set -uo pipefail

# ============================================================

# Dotfiles Installer

#

# Location:

# dotfiles/Scripts/Scripts/install-dotfiles.sh

#

# This script:

# 1. Detects the dotfiles repository.

# 2. Checks GNU Stow.

# 3. Checks software used by each configuration.

# 4. Shows available/missing software.

# 5. Shows all Stow packages that will be installed.

# 6. Shows existing files that will be overwritten.

# 7. Requires explicit confirmation.

# 8. Removes conflicting files from $HOME.

# 9. Restows every dotfiles package.

#

# WARNING:

# Existing configuration files matching this repository

# will be DELETED and replaced by symlinks to this repo.

# ============================================================

# ------------------------------------------------------------

# Colors

# ------------------------------------------------------------

if [[ -t 1 ]]; then
RED=$'\033[0;31m'
GREEN=$'\033[0;32m'
YELLOW=$'\033[1;33m'
BLUE=$'\033[0;34m'
BOLD=$'\033[1m'
RESET=$'\033[0m'
else
RED=""
GREEN=""
YELLOW=""
BLUE=""
BOLD=""
RESET=""
fi

# ------------------------------------------------------------

# Paths

# ------------------------------------------------------------

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

# Script lives at:

# dotfiles/Scripts/Scripts/install-dotfiles.sh

#

# Therefore repository root is two directories above.

REPO_ROOT="$(cd -- "$SCRIPT_DIR/../.." && pwd)"

TARGET="$HOME"

# ------------------------------------------------------------

# Helpers

# ------------------------------------------------------------

die() {
printf '%sERROR:%s %s\n' "$RED" "$RESET" "$*" >&2
exit 1
}

section() {
printf '\n%s%s==> %s%s\n' "$BOLD" "$BLUE" "$*" "$RESET"
}

success() {
printf '%s[OK]%s %s\n' "$GREEN" "$RESET" "$*"
}

warning() {
printf '%s[!!]%s %s\n' "$YELLOW" "$RESET" "$*"
}

missing() {
printf '%s[MISSING]%s %s\n' "$RED" "$RESET" "$*"
}

# ------------------------------------------------------------

# Validate repository

# ------------------------------------------------------------

section "Dotfiles repository"

printf 'Repository : %s\n' "$REPO_ROOT"
printf 'Target     : %s\n' "$TARGET"

[[ -d "$REPO_ROOT" ]] || die "Repository directory does not exist."

[[ -d "$REPO_ROOT/.git" ]] ||
warning "$REPO_ROOT does not appear to contain a .git directory."

# ------------------------------------------------------------

# Required dependency

# ------------------------------------------------------------

section "Required tools"

if command -v stow >/dev/null 2>&1; then
success "GNU Stow: $(command -v stow)"
else
missing "GNU Stow"
printf '\nInstall it first.\n'

if command -v pacman >/dev/null 2>&1; then
printf '\n  sudo pacman -S stow\n\n'
fi

exit 1
fi

if command -v git >/dev/null 2>&1; then
success "Git: $(command -v git)"
else
warning "Git is not installed."
fi

# ------------------------------------------------------------

# Software associated with each Stow package

# ------------------------------------------------------------

declare -A SOFTWARE=(
[alacritty]="alacritty"
[bash]="bash"
[conky]="conky"
[emacs]="emacs"
[foot]="foot"
[fuzzel]="fuzzel"
[guile]="guile"
[helix]="hx"
[hypr]="Hyprland"
[kanshi]="kanshi"
[lite-emacs]="emacs"
[mpv]="mpv"
[niri]="niri"
[nvim]="nvim"
[rofi]="rofi"
[swaync]="swaync"
[waybar]="waybar"
[wlogout]="wlogout"
[yazi]="yazi"
[zathura]="zathura"
[zsh]="zsh"
)

# ------------------------------------------------------------

# Find Stow packages

# ------------------------------------------------------------

section "Discovering Stow packages"

PACKAGES=()

while IFS= read -r dir; do
name="$(basename "$dir")"

case "$name" in
.git|docs)
continue
;;
esac

# Ignore empty directories.

if find "$dir" -mindepth 1 -print -quit | grep -q .; then
PACKAGES+=("$name")
fi
done < <(
find "$REPO_ROOT" 
-mindepth 1 
-maxdepth 1 
-type d 
-print |
sort
)

if ((${#PACKAGES[@]} == 0)); then
die "No Stow packages found."
fi

for package in "${PACKAGES[@]}"; do
printf '  %s\n' "$package"
done

printf '\nTotal: %d packages\n' "${#PACKAGES[@]}"

# ------------------------------------------------------------

# Check software

# ------------------------------------------------------------

section "Checking configured software"

MISSING_SOFTWARE=()

for package in "${PACKAGES[@]}"; do

command_name="${SOFTWARE[$package]:-}"

if [[ -z "$command_name" ]]; then
printf '%-14s %s\n' "$package" "configuration/scripts only"
continue
fi

if command -v "$command_name" >/dev/null 2>&1; then
printf '%s[OK]%s      %-14s %s\n' 
"$GREEN" 
"$RESET" 
"$package" 
"$(command -v "$command_name")"
else
printf '%s[MISSING]%s %-14s %s\n' 
"$RED" 
"$RESET" 
"$package" 
"$command_name"

```
MISSING_SOFTWARE+=("$package:$command_name")
```

fi

done

# ------------------------------------------------------------

# Missing software summary

# ------------------------------------------------------------

if ((${#MISSING_SOFTWARE[@]} > 0)); then

printf '\n%sSome configured programs are not installed:%s\n' 
"$YELLOW" "$RESET"

for item in "${MISSING_SOFTWARE[@]}"; do
package="${item%%:*}"
command_name="${item#*:}"

```
printf '  %-14s -> %s\n' "$package" "$command_name"
```

done

printf '\n'
warning "Their configuration can still be installed."
fi

# ------------------------------------------------------------

# Find conflicts

# ------------------------------------------------------------

section "Checking existing files"

CONFLICTS=()

for package in "${PACKAGES[@]}"; do

package_dir="$REPO_ROOT/$package"

while IFS= read -r -d '' source; do

```
relative="${source#"$package_dir"/}"
target="$TARGET/$relative"

# Absolute safety guard.
[[ -n "$relative" ]] || continue
[[ "$target" != "$TARGET" ]] || continue

if [[ -e "$target" || -L "$target" ]]; then

  # Do not count a link that already points into this package.
  if [[ -L "$target" ]]; then
    current_link="$(readlink -f -- "$target" 2>/dev/null || true)"
    source_link="$(readlink -f -- "$source" 2>/dev/null || true)"

    if [[ -n "$current_link" &&
          -n "$source_link" &&
          "$current_link" == "$source_link" ]]; then
      continue
    fi
  fi

  CONFLICTS+=("$target")
fi
```

done < <(
find "$package_dir" 
\(-type f -o -type l\) 
-print0
)

done

# ------------------------------------------------------------

# Show conflicts

# ------------------------------------------------------------

if ((${#CONFLICTS[@]} == 0)); then

success "No conflicting files found."

else

printf '%sThe following existing files/symlinks will be removed:%s\n\n' 
"$YELLOW" "$RESET"

printf '  %s\n' "${CONFLICTS[@]}"

printf '\nTotal conflicts: %d\n' "${#CONFLICTS[@]}"

fi

# ------------------------------------------------------------

# Final summary

# ------------------------------------------------------------

section "Installation summary"

printf 'Repository          : %s\n' "$REPO_ROOT"
printf 'Target              : %s\n' "$TARGET"
printf 'Stow packages       : %d\n' "${#PACKAGES[@]}"
printf 'Existing conflicts  : %d\n' "${#CONFLICTS[@]}"
printf 'Missing programs    : %d\n' "${#MISSING_SOFTWARE[@]}"

printf '\nPackages:\n'

printf '  %s\n' "${PACKAGES[@]}"

# ------------------------------------------------------------

# Destructive confirmation

# ------------------------------------------------------------

printf '\n'
printf '%s%sWARNING%s\n' "$BOLD" "$RED" "$RESET"
printf '%s\n' "Existing configuration files listed above WILL BE DELETED."
printf '%s\n' "The repository versions will replace them using GNU Stow."
printf '%s\n' "There is no automatic backup."

printf '\nType %sOVERWRITE%s to continue: ' "$BOLD" "$RESET"
read -r confirmation

if [[ "$confirmation" != "OVERWRITE" ]]; then
printf '\nInstallation cancelled.\n'
exit 0
fi

# ------------------------------------------------------------

# Remove conflicting files

# ------------------------------------------------------------

section "Removing conflicting files"

for target in "${CONFLICTS[@]}"; do

# Extra safety.

case "$target" in
"$HOME"|"/"|"")
die "Refusing to remove unsafe target: $target"
;;
esac

printf 'Removing %s\n' "$target"
rm -rf -- "$target"

done

# ------------------------------------------------------------

# Stow everything

# ------------------------------------------------------------

section "Installing dotfiles"

FAILED=()

for package in "${PACKAGES[@]}"; do

printf '\n%s-> %s%s\n' "$BLUE" "$package" "$RESET"

if stow 
--restow 
--dir="$REPO_ROOT" 
--target="$TARGET" 
"$package"
then
success "$package"
else
missing "Failed to stow $package"
FAILED+=("$package")
fi

done

# ------------------------------------------------------------

# Result

# ------------------------------------------------------------

section "Finished"

if ((${#FAILED[@]} > 0)); then

printf '%sThe following packages failed:%s\n' "$RED" "$RESET"

printf '  %s\n' "${FAILED[@]}"

exit 1

fi

success "All dotfiles installed successfully."

printf '\n'
printf 'Repository: %s\n' "$REPO_ROOT"
printf 'Target:     %s\n' "$TARGET"
printf '\n'

printf '%sDone.%s\n' "$GREEN" "$RESET"

```
```
