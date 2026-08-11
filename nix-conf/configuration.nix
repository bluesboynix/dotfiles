```nix
{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  # ------------------------------------------------------------
  # Bootloader
  # ------------------------------------------------------------

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # ------------------------------------------------------------
  # Kernel
  # ------------------------------------------------------------

  boot.kernelPackages = pkgs.linuxPackages_latest;

  # ------------------------------------------------------------
  # Filesystems
  #
  # vda1 = EFI
  # vda2 = Swap
  # vda3 = Root (ext4)
  #
  # These are normally already generated in
  # hardware-configuration.nix.
  # ------------------------------------------------------------

  fileSystems."/" = {
    device = "/dev/vda3";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/vda1";
    fsType = "vfat";
  };

  swapDevices = [
    {
      device = "/dev/vda2";
    }
  ];

  # ------------------------------------------------------------
  # AMD Ryzen 5 5500U / Radeon iGPU
  # ------------------------------------------------------------

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  # ------------------------------------------------------------
  # Networking
  # ------------------------------------------------------------

  networking.networkmanager.enable = true;

  # Optional hostname
  networking.hostName = "nixos";

  # ------------------------------------------------------------
  # Time zone
  # ------------------------------------------------------------

  time.timeZone = "Asia/Kolkata";

  # ------------------------------------------------------------
  # Locale
  # ------------------------------------------------------------

  i18n.defaultLocale = "en_US.UTF-8";

  console.keyMap = "us";

  # ------------------------------------------------------------
  # Niri Wayland compositor
  # ------------------------------------------------------------

  programs.niri.enable = true;

  # XWayland support for applications that still require X11
  environment.systemPackages = with pkgs; [
    xwayland-satellite
  ];

  # ------------------------------------------------------------
  # Ly display manager
  # ------------------------------------------------------------

  services.displayManager.ly.enable = true;

  # Do NOT enable:
  #
  # services.greetd.enable = true;
  # services.displayManager.gdm.enable = true;
  # services.displayManager.sddm.enable = true;
  #
  # Ly is the only display manager we want.
  # ------------------------------------------------------------

  # ------------------------------------------------------------
  # Audio - PipeWire
  # ------------------------------------------------------------

  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;

    alsa.enable = true;
    alsa.support32Bit = true;

    pulse.enable = true;
  };

  # ------------------------------------------------------------
  # Bluetooth
  # ------------------------------------------------------------

  hardware.bluetooth.enable = true;

  # ------------------------------------------------------------
  # Polkit
  #
  # Useful for graphical authentication dialogs under Niri.
  # ------------------------------------------------------------

  security.polkit.enable = true;

  # ------------------------------------------------------------
  # D-Bus
  # ------------------------------------------------------------

  services.dbus.enable = true;

  # ------------------------------------------------------------
  # User account
  # ------------------------------------------------------------

  users.users.YOUR_USERNAME = {
    isNormalUser = true;

    description = "YOUR_USERNAME";

    extraGroups = [
      "wheel"
      "networkmanager"
      "video"
      "audio"
    ];

    packages = with pkgs; [
      # User packages can go here
    ];
  };

  # ------------------------------------------------------------
  # Shell
  # ------------------------------------------------------------

  programs.zsh.enable = true;

  users.defaultUserShell = pkgs.zsh;

  environment.shells = [
    pkgs.bash
    pkgs.zsh
  ];

  # ------------------------------------------------------------
  # System packages
  # ------------------------------------------------------------

  environment.systemPackages = with pkgs; [

    # Terminal
    foot

    # Basic tools
    git
    curl
    wget

    # Editors
    vim
    nano

    # File / system utilities
    tree
    ripgrep
    fd
    unzip
    zip

    # System monitoring
    btop
    htop

    # Network tools
    networkmanager
    iw

    # Wayland utilities
    wl-clipboard
    wayland-utils

    # XWayland compatibility
    xwayland-satellite

    # Audio control
    pavucontrol

    # Bluetooth
    blueman

    # GPU diagnostics
    vulkan-tools
    mesa-demos
  ];

  # ------------------------------------------------------------
  # XDG / Wayland desktop integration
  # ------------------------------------------------------------

  xdg.portal = {
    enable = true;

    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
    ];
  };

  # ------------------------------------------------------------
  # Fonts
  # ------------------------------------------------------------

  fonts.packages = with pkgs; [
    nerd-fonts.fira-code
    nerd-fonts.jetbrains-mono
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-emoji
  ];

  # ------------------------------------------------------------
  # Printing
  # ------------------------------------------------------------

  services.printing.enable = true;

  # ------------------------------------------------------------
  # SSH
  #
  # Uncomment if you want SSH server access.
  # ------------------------------------------------------------

  # services.openssh.enable = true;

  # ------------------------------------------------------------
  # Firmware
  # ------------------------------------------------------------

  hardware.enableRedistributableFirmware = true;

  # ------------------------------------------------------------
  # Nix settings
  # ------------------------------------------------------------

  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];

    auto-optimise-store = true;
  };

  # ------------------------------------------------------------
  # Automatic garbage collection
  # ------------------------------------------------------------

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 14d";
  };

  # ------------------------------------------------------------
  # State version
  #
  # IMPORTANT:
  # Keep this equal to the NixOS version you installed.
  # For NixOS 26.05 use:
  # ------------------------------------------------------------

  system.stateVersion = "26.05";
}
```
