# My Dotfiles

Personal, modular dotfiles and scripts managed with [GNU Stow](https://www.gnu.org/software/stow/). Clean, minimal, and easily reproducible across systems.


## 🛠 Structure

Each application or tool has its own folder and mirrors its target location in `$HOME`. This makes it easy to symlink everything with `stow`.

## 📦 Installation
Clone the repo:
```bash
git clone https://github.com/bluesboynix/dotfiles.git ~/dotfiles
cd ~/dotfiles
```
# Symlink configs with GNU Stow:
```bash
stow emacs foot hypr nvim rofi waybar wlogout yazi zathura zsh
```
# Requirements
* GNU Stow (sudo pacman -S stow on Arch)
* A Unix-like system with configs under ~/.config/
# Configured Applications
* emacs – main editor
* foot – Wayland terminal
* hypr – Hyprland WM
* nvim – Neovim config
* rofi – app launcher
* waybar – status bar
* wlogout – logout menu
* yazi – TUI file manager
* zathura – minimalist PDF viewer
* zsh – shell configuration
