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
* emacs
  - Core system and utilities - git ripgrep fd make gcc tree exa
  - Fonts - ttf-fira-code ttf-dejavu
  - Programming languages - sbcl python go nodejs npm
  - Rust - https://rust-lang.org/tools/install/
 
| Category                      | Tool                    | Arch Package                | Purpose                                               |
| ----------------------------- | ----------------------- | --------------------------- | ----------------------------------------------------- |
| **Terminal Integration**      | **vterm**               | `emacs-libvterm`            | Terminal emulator inside Emacs                        |
| **Version Control**           | **Git**                 | `git`                       | Required for Magit, Projectile, etc.                  |
| **Common Lisp**               | **SBCL**                | `sbcl`                      | Default Common Lisp implementation for SLIME          |
|                               | **Quicklisp**           | *(manual install)*          | SLIME helper and Lisp libraries                       |
| **Compilation Tools**         | **make**, **gcc**       | `base-devel`                | Required for native compilation / packages like vterm |
| **Icons / Fonts**             | **all-the-icons fonts** | `ttf-all-the-icons` *(AUR)* | Needed for `doom-modeline`, `treemacs`, etc.          |
| **Language Tools (Optional)** |                         |                             |                                                       |
| C/C++                         | `clang`, `gdb`          | `clang`, `gdb`              | For `lsp-mode` or C/C++ dev                           |
| Python                        | `python`, `black`       | `python`, `python-black`    | For Python support                                    |
| Rust                          | `rustup`                | `rustup`                    | For `rust-mode` (install toolchains via `rustup`)     |
| **Web-mode (optional)**       | `nodejs`                | `nodejs`                    | Required if using LSP or formatters for web files     |

* foot – Wayland terminal
  - Font - Hack nerd font
* hypr – Hyprland WM
  - https://wiki.archlinux.org/title/Hyprland
* nvim – Neovim config
  - better use nvchad or LazyVim
* rofi – app launcher
* waybar – status bar
* wlogout – logout menu
* yazi – TUI file manager
* zathura – minimalist PDF viewer
* zsh – shell configuration
