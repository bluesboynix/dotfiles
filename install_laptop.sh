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

echo "[*] Backing up and linking configuration files..."

# Create .config directory if it doesn't exist
mkdir -p "$HOME/.config"

# List of directories to symlink entirely (except nvim)
DIRECTORY_LINKS=("foot" "hypr" "rofi" "waybar" "wlogout" "yazi" "zathura")

for dir in "${DIRECTORY_LINKS[@]}"; do
    TARGET="$HOME/.config/$dir"
    SOURCE="$TARGET_DIR/$BRANCH/config/$dir"

    if [ -e "$TARGET" ] || [ -L "$TARGET" ]; then
        mkdir -p "$BACKUP_DIR/.config"
        mv -v "$TARGET" "$BACKUP_DIR/.config/$dir"
    fi

    ln -s "$SOURCE" "$TARGET"
    echo "✓ Linked directory .config/$dir"
done

# Special handling for nvim (file-level symlinks)
echo "[*] Setting up neovim configuration (file-level links)..."
mkdir -p "$HOME/.config/nvim"
NVIM_FILES=(
    "init.lua"
    "lua/config/keymaps.lua"
    "lua/config/lazy.lua"
    "lua/config/vim-options.lua"
    "lua/plugins/alpha-nvim.lua"
    "lua/plugins/bufferline.lua"
    "lua/plugins/colorscheme.lua"
    "lua/plugins/completions.lua"
    "lua/plugins/dap-go.lua"
    "lua/plugins/indent-blankline.lua"
    "lua/plugins/lazygit.lua"
    "lua/plugins/lualine.lua"
    "lua/plugins/mason-lspconfig.lua"
    "lua/plugins/mason.lua"
    "lua/plugins/none-ls.lua"
    "lua/plugins/nvim-autopairs.lua"
    "lua/plugins/nvim-lspconfig.lua"
    "lua/plugins/nvim-tree.lua"
    "lua/plugins/nvim-ufo.lua"
    "lua/plugins/smart-splits.lua"
    "lua/plugins/telescope.lua"
    "lua/plugins/toggleterm.lua"
    "lua/plugins/treesitter.lua"
)

for file in "${NVIM_FILES[@]}"; do
    TARGET="$HOME/.config/nvim/$file"
    SOURCE="$TARGET_DIR/$BRANCH/config/nvim/$file"
    
    # Create parent directory if needed
    mkdir -p "$(dirname "$TARGET")"
    
    if [ -e "$TARGET" ] || [ -L "$TARGET" ]; then
        mkdir -p "$BACKUP_DIR/.config/nvim/$(dirname "$file")"
        mv -v "$TARGET" "$BACKUP_DIR/.config/nvim/$file"
    fi

    ln -s "$SOURCE" "$TARGET"
    echo "✓ Linked nvim/$file"
done



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

echo "[*] Linking shell configuration files..."

# bashrc
if [ -e "$HOME/.bashrc" ] || [ -L "$HOME/.bashrc" ]; then
    mv -v "$HOME/.bashrc" "$BACKUP_DIR/bashrc"
fi
ln -s "$TARGET_DIR/$BRANCH/bashrc" "$HOME/.bashrc"
echo "✓ Linked .bashrc"

# zshrc
if [ -e "$HOME/.zshrc" ] || [ -L "$HOME/.zshrc" ]; then
    mv -v "$HOME/.zshrc" "$BACKUP_DIR/zshrc"
fi
ln -s "$TARGET_DIR/$BRANCH/zshrc" "$HOME/.zshrc"
echo "✓ Linked .zshrc"

echo "[✔] Dotfiles installed successfully."
echo "[!] Old configurations backed up to: $BACKUP_DIR"
