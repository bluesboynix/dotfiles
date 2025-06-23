#!/bin/bash
set -e

# Variables
REPO_URL="https://github.com/bluesboynix/dotfiles.git"
TARGET_DIR="$HOME/.dotfiles_pc"
BRANCH="pc"

echo "[*] Cloning pc branch of dotfiles..."
git clone --branch "$BRANCH" "$REPO_URL" "$TARGET_DIR"

echo "[*] Backing up and linking files..."

# .config
if [ -e "$HOME/.config" ] || [ -L "$HOME/.config" ]; then
  mv "$HOME/.config" "$HOME/.config.backup.$(date +%s)"
fi
ln -s "$TARGET_DIR/pc/config" "$HOME/.config"
echo "✓ Linked .config"

# .emacs.d
if [ -e "$HOME/.emacs.d" ] || [ -L "$HOME/.emacs.d" ]; then
  mv "$HOME/.emacs.d" "$HOME/.emacs.d.backup.$(date +%s)"
fi
ln -s "$TARGET_DIR/pc/emacs.d" "$HOME/.emacs.d"
echo "✓ Linked .emacs.d"

# .bashrc
if [ -e "$HOME/.bashrc" ] || [ -L "$HOME/.bashrc" ]; then
  mv "$HOME/.bashrc" "$HOME/.bashrc.backup.$(date +%s)"
fi
ln -s "$TARGET_DIR/pc/bashrc" "$HOME/.bashrc"
echo "✓ Linked .bashrc"

# .zshrc
if [ -e "$HOME/.zshrc" ] || [ -L "$HOME/.zshrc" ]; then
  mv "$HOME/.zshrc" "$HOME/.zshrc.backup.$(date +%s)"
fi
ln -s "$TARGET_DIR/pc/zshrc" "$HOME/.zshrc"
echo "✓ Linked .zshrc"

echo "[✔] PC dotfiles installed successfully."

