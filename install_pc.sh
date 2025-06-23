#!/bin/bash
set -e

REPO_URL="https://github.com/bluesboynix/dotfiles.git"
TARGET_DIR="$HOME/.dotfiles_pc"  # change to .dotfiles_pc if installing pc branch
BRANCH="pc"

echo "[*] Cloning $BRANCH branch of dotfiles..."
git clone --branch "$BRANCH" "$REPO_URL" "$TARGET_DIR"

echo "[*] Backing up and linking selected .config subfolders..."

mkdir -p "$HOME/.config"

for folder in foot hypr nvim rofi waybar wlogout yazi zathura; do
  TARGET="$HOME/.config/$folder"
  SOURCE="$TARGET_DIR/$BRANCH/config/$folder"

  if [ -e "$TARGET" ] || [ -L "$TARGET" ]; then
    mv "$TARGET" "${TARGET}.backup.$(date +%s)"
  fi

  ln -s "$SOURCE" "$TARGET"
  echo "✓ Linked $folder in .config"
done

echo "[*] Backing up and linking init.el and custom.el for Emacs..."

mkdir -p "$HOME/.emacs.d"

for file in init.el custom.el; do
  TARGET="$HOME/.emacs.d/$file"
  SOURCE="$TARGET_DIR/$BRANCH/emacs/$file"

  if [ -e "$TARGET" ] || [ -L "$TARGET" ]; then
    mv "$TARGET" "${TARGET}.backup.$(date +%s)"
  fi

  ln -s "$SOURCE" "$TARGET"
  echo "✓ Linked $file"
done

# bashrc
if [ -e "$HOME/.bashrc" ] || [ -L "$HOME/.bashrc" ]; then
  mv "$HOME/.bashrc" "$HOME/.bashrc.backup.$(date +%s)"
fi
ln -s "$TARGET_DIR/$BRANCH/bashrc" "$HOME/.bashrc"
echo "✓ Linked .bashrc"

# zshrc
if [ -e "$HOME/.zshrc" ] || [ -L "$HOME/.zshrc" ]; then
  mv "$HOME/.zshrc" "$HOME/.zshrc.backup.$(date +%s)"
fi
ln -s "$TARGET_DIR/$BRANCH/zshrc" "$HOME/.zshrc"
echo "✓ Linked .zshrc"

echo "[✔] Dotfiles installed successfully."
