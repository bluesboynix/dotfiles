#!/bin/bash
set -e

REPO_URL="https://github.com/bluesboynix/dotfiles.git"
TARGET_DIR="$HOME/.dotfiles_pc"  # or .dotfiles_pc for PC branch
BRANCH="laptop"
BACKUP_DIR="$HOME/.dotfiles_backup/$(date +%Y%m%d_%H%M%S)"

echo "[*] Creating backup directory at $BACKUP_DIR"
mkdir -p "$BACKUP_DIR"

echo "[*] Cloning $BRANCH branch of dotfiles..."
if [ ! -d "$TARGET_DIR" ]; then
    git clone --branch "$BRANCH" "$REPO_URL" "$TARGET_DIR"
else
    echo "✓ Repository already exists at $TARGET_DIR"
fi

# Verify the repository structure
echo "[*] Verifying repository structure..."
if [ ! -d "$TARGET_DIR/$BRANCH/emacs.d" ]; then
    echo "Error: emacs.d directory not found in repository!"
    exit 1
fi

echo "[*] Setting up Emacs configuration..."
mkdir -p "$HOME/.emacs.d"

# Function to safely create symlinks
create_symlink() {
    local source=$1
    local target=$2
    local backup_dir=$3
    
    if [ ! -f "$source" ]; then
        echo "Warning: Source file $source does not exist!"
        return 1
    fi
    
    if [ -e "$target" ] || [ -L "$target" ]; then
        mkdir -p "$backup_dir"
        mv -v "$target" "$backup_dir/$(basename "$target")"
    fi
    
    ln -s "$source" "$target" && echo "✓ Linked $(basename "$target")"
}

# Handle Emacs files
EMACS_FILES=("init.el" "custom.el")
for file in "${EMACS_FILES[@]}"; do
    SOURCE="$TARGET_DIR/$BRANCH/emacs.d/$file"
    TARGET="$HOME/.emacs.d/$file"
    create_symlink "$SOURCE" "$TARGET" "$BACKUP_DIR/.emacs.d"
done

# Verify symlinks were created successfully
echo "[*] Verifying symlinks..."
for file in "${EMACS_FILES[@]}"; do
    TARGET="$HOME/.emacs.d/$file"
    if [ ! -L "$TARGET" ] || [ ! -e "$TARGET" ]; then
        echo "Error: Failed to create valid symlink for $TARGET"
        exit 1
    else
        echo "✓ Verified $file symlink"
    fi
done

echo "[✔] Emacs configuration installed successfully."
