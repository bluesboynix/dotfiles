#!/bin/bash
set -eo pipefail

# Configuration
REPO_URL="https://github.com/bluesboynix/dotfiles.git"
TARGET_DIR="$HOME/.dotfiles_laptop"
BRANCH="laptop"
BACKUP_DIR="$HOME/.dotfiles_backup/$(date +%Y%m%d_%H%M%S)"
LOG_FILE="$BACKUP_DIR/install.log"
EMACS_CONFIG_DIR="$HOME/.config/emacs"  # XDG location

# Initialize logging
mkdir -p "$BACKUP_DIR"
exec > >(tee -a "$LOG_FILE") 2>&1

echo "[*] Dotfiles installation started at $(date)"
echo "[*] Backup directory: $BACKUP_DIR"
echo "[*] Log file: $LOG_FILE"

# Symlink function (unchanged)
create_symlink() {
    local source=$1
    local target=$2
    local backup_dir=$3
    
    if [ ! -e "$source" ]; then
        echo "  ⚠️  Warning: Source $source does not exist!"
        return 1
    fi
    
    if [ -e "$target" ] || [ -L "$target" ]; then
        mkdir -p "$backup_dir"
        echo "  🔄 Backing up existing $(basename "$target")"
        mv -v "$target" "$backup_dir/$(basename "$target")"
    fi
    
    mkdir -p "$(dirname "$target")"
    
    if ln -s "$source" "$target"; then
        echo "  ✓ Linked $(basename "$target")"
    else
        echo "  ❌ Failed to link $(basename "$target")"
        return 1
    fi
}

# Clone/update repository
if [ ! -d "$TARGET_DIR" ]; then
    echo "[*] Cloning $BRANCH branch..."
    git clone --branch "$BRANCH" "$REPO_URL" "$TARGET_DIR"
else
    echo "[*] Updating existing repository..."
    cd "$TARGET_DIR" && git pull
fi

# Verify repository structure
echo "[*] Verifying repository structure..."
REQUIRED_DIRS=("$BRANCH/config" "$BRANCH/config/emacs")
for dir in "${REQUIRED_DIRS[@]}"; do
    if [ ! -d "$TARGET_DIR/$dir" ]; then
        echo "❌ Error: Required directory $dir not found!"
        exit 1
    fi
done

# Process Emacs config (now under config/emacs)
echo "[*] Setting up Emacs configuration..."
mkdir -p "$EMACS_CONFIG_DIR"
EMACS_FILES=("init.el" "custom.el")
for file in "${EMACS_FILES[@]}"; do
    create_symlink \
        "$TARGET_DIR/$BRANCH/config/emacs/$file" \
        "$EMACS_CONFIG_DIR/$file" \
        "$BACKUP_DIR/.config/emacs"
done

# Process shell configuration
echo "[*] Setting up shell configuration..."
declare -A SHELL_FILES=(
    ["bashrc"]="$TARGET_DIR/$BRANCH/bashrc"
    ["zshrc"]="$TARGET_DIR/$BRANCH/zshrc"
)

for target in "${!SHELL_FILES[@]}"; do
    create_symlink "${SHELL_FILES[$target]}" "$HOME/.$target" "$BACKUP_DIR"
done

# Verification
echo "[*] Verifying installations..."
FAILED=0

verify_installation() {
    local target=$1
    if [[ ! -e "$target" ]] || [[ ! -L "$target" ]]; then
        echo "  ❌ Verification failed for $target"
        return 1
    fi
    return 0
}

# Verify all symlinks
for dir in ${CONFIG_TYPES["directory"]}; do
    verify_installation "$HOME/.config/$dir" || FAILED=1
done

for file in "${EMACS_FILES[@]}"; do
    verify_installation "$EMACS_CONFIG_DIR/$file" || FAILED=1
done

for target in "${!SHELL_FILES[@]}"; do
    verify_installation "$HOME/.$target" || FAILED=1
done

[[ $FAILED -eq 0 ]] || {
    echo "❌ Some installations failed verification"
    exit 1
}

echo "[✔] Dotfiles installed successfully"
echo "    Backup: $BACKUP_DIR"
echo "    Log: $LOG_FILE"
echo "    Emacs config: $EMACS_CONFIG_DIR"
exit 0
