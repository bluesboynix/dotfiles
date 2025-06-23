#!/bin/bash
set -eo pipefail

# Configuration
REPO_URL="https://github.com/bluesboynix/dotfiles.git"
TARGET_DIR="$HOME/.dotfiles_pc"
BRANCH="pc"
BACKUP_DIR="$HOME/.dotfiles_backup/$(date +%Y%m%d_%H%M%S)"
LOG_FILE="$BACKUP_DIR/install.log"
EMACS_CONFIG_DIR="$HOME/.config/emacs"  # XDG location for Emacs 30+

# Initialize logging
mkdir -p "$BACKUP_DIR"
exec > >(tee -a "$LOG_FILE") 2>&1

echo "[*] Dotfiles installation started at $(date)"
echo "[*] Emacs 30.1 detected - Using XDG config exclusively"
echo "[*] Backup directory: $BACKUP_DIR"
echo "[*] Log file: $LOG_FILE"

# Improved symlink function
create_symlink() {
    local source=$1
    local target=$2
    local backup_dir=$3
    
    # Verify source exists
    [[ -e "$source" ]] || {
        echo "  ⚠️  Warning: Source $source does not exist!"
        return 1
    }
    
    # Backup if target exists
    if [[ -e "$target" ]] || [[ -L "$target" ]]; then
        mkdir -p "$backup_dir"
        echo "  🔄 Backing up existing $(basename "$target")"
        mv -v "$target" "$backup_dir/$(basename "$target")" || return 1
    fi
    
    # Create parent directory if needed
    mkdir -p "$(dirname "$target")" || return 1
    
    # Create symlink
    if ln -s "$source" "$target"; then
        echo "  ✓ Linked $(basename "$target")"
    else
        echo "  ❌ Failed to link $(basename "$target")"
        return 1
    fi
}

# Clone or update repository
if [[ ! -d "$TARGET_DIR" ]]; then
    echo "[*] Cloning $BRANCH branch..."
    git clone --branch "$BRANCH" "$REPO_URL" "$TARGET_DIR"
else
    echo "[*] Updating existing repository..."
    cd "$TARGET_DIR" && git pull
fi

# Verify repository structure
echo "[*] Verifying repository structure..."
REQUIRED_DIRS=("$BRANCH/config" "$BRANCH/emacs")  # Note: 'emacs' not 'emacs.d'
for dir in "${REQUIRED_DIRS[@]}"; do
    [[ -d "$TARGET_DIR/$dir" ]] || {
        echo "❌ Error: Required directory $dir not found!"
        exit 1
    }
done

# Configuration types
declare -A CONFIG_TYPES=(
    ["directory"]="foot hypr rofi waybar wlogout yazi zathura"
    ["file"]="nvim"
)

# Process directory-level symlinks
echo "[*] Processing directory-level symlinks..."
for dir in ${CONFIG_TYPES["directory"]}; do
    echo "  Processing $dir..."
    create_symlink "$TARGET_DIR/$BRANCH/config/$dir" "$HOME/.config/$dir" "$BACKUP_DIR/.config"
done

# Process file-level symlinks (Neovim)
echo "[*] Processing Neovim configuration..."
while IFS= read -r -d '' source_file; do
    rel_path="${source_file#$TARGET_DIR/$BRANCH/config/nvim/}"
    create_symlink "$source_file" "$HOME/.config/nvim/$rel_path" "$BACKUP_DIR/.config/nvim"
done < <(find "$TARGET_DIR/$BRANCH/config/nvim" -type f \( -name "*.lua" -o -name "init.lua" \) -print0)

# Process Emacs configuration (XDG-only for Emacs 30+)
echo "[*] Setting up Emacs (XDG-only)..."
mkdir -p "$EMACS_CONFIG_DIR"

EMACS_FILES=("init.el" "early-init.el" "custom.el")
for file in "${EMACS_FILES[@]}"; do
    [[ -f "$TARGET_DIR/$BRANCH/emacs/$file" ]] && 
        create_symlink "$TARGET_DIR/$BRANCH/emacs/$file" "$EMACS_CONFIG_DIR/$file" "$BACKUP_DIR/.config/emacs"
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
