#!/bin/bash
set -e

# Variables
DOTFILES_REPO="https://github.com/bluesboynix/dotfiles.git"
DOTFILES_DIR="$HOME/dotfiles"
CONFIG_DIR="$HOME/.config"
BACKUP_DIR="$HOME/dotfiles_backup_$(date +%Y%m%d%H%M%S)"

# Step 1: Clone repo if it doesn't exist
if [ ! -d "$DOTFILES_DIR" ]; then
  echo "Cloning dotfiles repo..."
  git clone "$DOTFILES_REPO" "$DOTFILES_DIR"
else
  echo "Dotfiles repo already exists, pulling latest changes..."
  cd "$DOTFILES_DIR" && git pull
fi

# Step 2: Backup existing configs
echo "Backing up existing configs to $BACKUP_DIR"
mkdir -p "$BACKUP_DIR"

declare -a folders=(
  "nvim"
  "foot"
  "hypr"
  "rofi"
  "waybar"
  "wlogout"
  "yazi"
  "zathura"
)

for folder in "${folders[@]}"; do
  TARGET="$CONFIG_DIR/$folder"
  if [ -e "$TARGET" ] || [ -L "$TARGET" ]; then
    mv "$TARGET" "$BACKUP_DIR/"
    echo "Backed up $TARGET"
  fi
done

# Backup .zshrc
if [ -e "$HOME/.zshrc" ] || [ -L "$HOME/.zshrc" ]; then
  mv "$HOME/.zshrc" "$BACKUP_DIR/"
  echo "Backed up ~/.zshrc"
fi

# Step 3: Create symlinks
for folder in "${folders[@]}"; do
  TARGET="$CONFIG_DIR/$folder"
  SOURCE="$DOTFILES_DIR/.config/$folder"
  ln -s "$SOURCE" "$TARGET"
  echo "Linked $TARGET -> $SOURCE"
done

ln -s "$DOTFILES_DIR/.zshrc" "$HOME/.zshrc"
echo "Linked ~/.zshrc -> $DOTFILES_DIR/.zshrc"

echo "Dotfiles installation complete!"
