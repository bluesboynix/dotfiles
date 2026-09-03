# My Dotfiles

Personal Linux dotfiles for a clean, keyboard-driven development environment.

Managed with **GNU Stow**, these configurations cover my shell, terminal, editors, Wayland compositors, status bars, utilities, media applications, and development tools.

> **Note:** These are my personal configurations. Feel free to copy anything useful, but don't expect everything to work unchanged on your system.

---

## ✨ What's Inside

| Directory     | Purpose                               |
| ------------- | ------------------------------------- |
| `alacritty/`  | Alacritty terminal configuration      |
| `bash/`       | Bash configuration                    |
| `conky/`      | Conky system monitor configuration    |
| `emacs/`      | Main Emacs configuration              |
| `foot/`       | Foot Wayland terminal                 |
| `fuzzel/`     | Fuzzel application launcher           |
| `guile/`      | Guile Scheme configuration            |
| `helix/`      | Helix editor configuration            |
| `hypr/`       | Hyprland configuration                |
| `kanshi/`     | Dynamic display configuration         |
| `lite-emacs/` | Minimal Emacs configuration           |
| `mpv/`        | MPV configuration                     |
| `niri/`       | Niri Wayland compositor configuration |
| `nvim/`       | Neovim configuration                  |
| `rofi/`       | Rofi launcher configuration           |
| `Scripts/`    | Personal utility scripts              |
| `swaync/`     | SwayNC notification configuration     |
| `waybar/`     | Waybar status bar                     |
| `wlogout/`    | Wayland logout menu                   |
| `yazi/`       | Yazi terminal file manager            |
| `zathura/`    | Zathura PDF/document viewer           |
| `zsh/`        | Zsh configuration                     |

---

## 🖥️ Desktop

The primary desktop environment in these dotfiles is built around **Wayland**.

### Niri

[Niri](https://github.com/YaLTeR/niri) is the main compositor configuration.

The setup includes configuration for:

* Window management
* Keybindings
* Workspaces
* Applications
* Screenshots
* Clipboard
* Notifications
* Status bar
* Session/logout controls

### Hyprland

A separate Hyprland configuration is also included for systems where I want to use Hyprland instead of Niri.

### Supporting tools

* **Waybar** — status bar
* **SwayNC** — notifications
* **Fuzzel / Rofi** — application launchers
* **Wlogout** — logout/shutdown menu
* **Kanshi** — display profiles
* **Conky** — system information

---

## 🖥️ Terminals

### Foot

My preferred Wayland terminal.

```text
foot/
└── .config/
    └── foot/
```

### Alacritty

An additional terminal configuration is included for systems where Alacritty is preferred.

---

## 📝 Editors

### Emacs

The main Emacs configuration lives in:

```text
emacs/.config/emacs/
```

It is organized into separate modules for things such as:

* Core configuration
* UI
* Project management
* LSP / Eglot
* Diagnostics
* Formatting
* Programming languages
* Completion
* Terminal integration

The configuration is intended to provide a full development environment while keeping the individual components modular.

### Lite Emacs

There is also a deliberately minimal configuration:

```text
lite-emacs/.config/lite-emacs/
```

The idea is simple:

> **Full Emacs when I want an IDE. Lite Emacs when I just want an editor.**

The lightweight configuration avoids the larger package setup and is useful for quickly opening files from a terminal.

### Neovim

The repository also contains a Neovim configuration.

```text
nvim/.config/nvim/
```

### Helix

A Helix configuration is included as another lightweight editor option.

---

## 🐚 Shell

The repository contains configurations for:

* Bash
* Zsh

My primary shell setup is Zsh.

The shell configuration contains aliases, environment configuration, prompts, and other command-line conveniences.

---

## 📦 Installation

Clone the repository:

```bash
git clone https://github.com/bluesboynix/dotfiles.git ~/dotfiles
cd ~/dotfiles
```

Install GNU Stow if it isn't already installed.

### Arch Linux

```bash
sudo pacman -S stow
```

Then choose the configurations you want:

```bash
stow zsh
stow foot
stow niri
stow waybar
stow emacs
```

Or install several at once:

```bash
stow zsh foot niri waybar emacs fuzzel swaync wlogout yazi zathura
```

---

## 🔗 How GNU Stow Works

Each directory is structured to mirror `$HOME`.

For example:

```text
zsh/
└── .zshrc
```

Running:

```bash
stow zsh
```

creates:

```text
~/.zshrc -> ~/dotfiles/zsh/.zshrc
```

Likewise:

```text
niri/
└── .config/
    └── niri/
        └── config.kdl
```

becomes:

```text
~/.config/niri/config.kdl
    ↓
~/dotfiles/niri/.config/niri/config.kdl
```

This means the files remain version-controlled while the applications use them from their normal locations.

---

## 🔄 Updating

Pull the latest changes:

```bash
cd ~/dotfiles
git pull
```

Because the configurations are symlinked by Stow, changes made inside the repository are immediately reflected in the corresponding configuration files.

---

## 🧹 Removing a Configuration

To remove a Stow package:

```bash
stow -D niri
```

For example:

```bash
stow -D emacs
```

This removes the symlinks without deleting the files from the repository.

---

## 🛠️ Recommended Base Packages

The exact requirements depend on which parts of the repository you use.

A typical Arch Linux setup may include:

```bash
sudo pacman -S \
    git \
    stow \
    zsh \
    foot \
    fuzzel \
    waybar \
    swaync \
    wlogout \
    yazi \
    zathura \
    mpv \
    conky
```

For development, additional tools such as:

```text
gcc
make
git
ripgrep
fd
curl
wget
```

and language-specific toolchains may be required.

---

## 📁 Repository Layout

```text
dotfiles/
├── Scripts/
├── alacritty/
├── bash/
├── conky/
├── docs/
├── emacs/
├── foot/
├── fuzzel/
├── guile/
├── helix/
├── hypr/
├── kanshi/
├── lite-emacs/
├── mpv/
├── niri/
├── nvim/
├── rofi/
├── swaync/
├── waybar/
├── wlogout/
├── yazi/
├── zathura/
├── zsh/
└── README.md
```

---

## 🎯 Philosophy

These dotfiles are intentionally **modular**.

I don't want one giant configuration that has to be installed as a complete package. Each application has its own Stow package, so I can selectively install what I need.

The general goals are:

* Minimal configuration
* Keyboard-driven workflow
* Wayland-first desktop
* Terminal-focused development
* Modular configuration
* Reproducibility
* Easy experimentation
* No unnecessary dependencies

---

## ⚠️ Disclaimer

These configurations are primarily designed for my own systems.

They may assume:

* Arch Linux or a similar Linux distribution
* Wayland
* GNU Stow
* Certain applications being installed
* Specific fonts
* Personal keybindings
* Personal directory layouts

If something doesn't work on your machine, check the relevant configuration before copying the entire repository.

---

## 📜 License

Use, modify, and copy anything useful from these dotfiles.
