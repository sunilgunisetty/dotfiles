{ config, pkgs, ... }:
let
  kube_prompt = builtins.fetchGit {url = "https://github.com/jonmosco/kube-ps1.git";};
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "sgunisetty";
  home.homeDirectory = "/Users/sgunisetty";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.09";

  home.packages = with pkgs; [
    htop
    jq
    bat
    awscli2
    jdk11
    (leiningen.override { jdk = jdk11; })
    rlwrap
    kubectl
    docker
    vault
    terraform_0_13
    ag
    nodejs
    fd
    git
    (python39.withPackages (ps: with ps; [ pip ]))
    ruby_2_7
    go
    rustup
    ghc
    stack
    clojure
    telnet
    coreutils
    erlang
    elixir
  ];

  programs.emacs = {
    enable = true;
    extraPackages = (epkgs:
      (with epkgs; [
        nix-mode
        magit
        magit-gitflow
        diff-hl
        git-link
        forge
        ag
        use-package
        all-the-icons
        which-key
        all-the-icons
        highlight-indent-guides
        projectile
        counsel-projectile
        counsel
        flx
        all-the-icons-ivy
        multiple-cursors
        expand-region
        paredit
        paredit-everywhere
        highlight-parentheses
        rainbow-delimiters
        flycheck-clj-kondo
        company
        clojure-mode
        cider
        clj-refactor
        org
        org-bullets
        wgrep-ag
        doom-modeline
        prettier-js
        js2-mode
        rjsx-mode
        json-mode
        kubernetes
        markdown-mode
        yaml-mode
        terraform-mode
        speed-type
        ample-theme
      ]));
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    defaultKeymap = "emacs";
    history.extended = true;
    oh-my-zsh = {
      enable = true;
      theme = "agnoster";
      plugins = [
        "git"
        "terraform"
        "kubectl"
        "docker"
      ];
    };
    shellAliases = {
      uuid = "uuidgen | awk '{print tolower($0)}' |  tr -d \\n";
      ll = "ls -la";
      e = "emacs -nw";
      tf = "terraform";
      switch = "home-manager switch && source ~/.zshrc";
      cat = "bat";
      grep = "grep --color=auto";
      zrc = "bat ~/.zshrc";
    };
    initExtraFirst = ''
      if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
         . $HOME/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

      if [ -e ~/.zshenv ]; then
        source ~/.zshenv
      fi

      # extra private config
      if [ -e ~/.env ]; then
         source ~/.env
      fi

      # Kube Prompt from https://github.com/jonmosco/kube-ps1
      if [ -e ${kube_prompt}/kube-ps1.sh ]; then
         source ${kube_prompt}/kube-ps1.sh
      fi
    '';
    initExtra = ''
      # Have to put this at the end of file to make other function available in the environment
      PROMPT='$(kube_ps1)'$PROMPT
    '';
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.kitty = {
    enable = true;
    settings = {
      font_size = "13";
      disable_ligature = "never";
      cursor_shape = "beam";
      tab_bar_style = "separator";
      macos_titlebar_color = "system";
      macos_option_as_alt = "yes";
    };
    font = {
      package = pkgs.fira-code;
      name = "Fira Code Retina";
    };
    keybindings = {
      "cmd+right" = "next_tab";
      "cmd+left" = "previous_tab";
    };
    extraConfig = ''
      # Base16 Tomorrow Night - kitty color config
      # Scheme by Chris Kempson (http://chriskempson.com)
      background #1d1f21
      foreground #c5c8c6
      selection_background #c5c8c6
      selection_foreground #1d1f21
      url_color #b4b7b4
      cursor #c5c8c6
      active_border_color #969896
      inactive_border_color #282a2e
      tab_bar_background #282a2e

      active_tab_foreground   #282c34
      active_tab_background   #abb2bf
      active_tab_font_style   bold
      inactive_tab_foreground #5c6370
      inactive_tab_background #22262d
      inactive_tab_font_style normal

      # normal
      color0 #1d1f21
      color1 #cc6666
      color2 #b5bd68
      color3 #f0c674
      color4 #81a2be
      color5 #b294bb
      color6 #8abeb7
      color7 #c5c8c6

      # bright
      color8 #969896
      color9 #cc6666
      color10 #b5bd68
      color11 #f0c674
      color12 #81a2be
      color13 #b294bb
      color14 #8abeb7
      color15 #ffffff

      # extended base16 colors
      color16 #de935f
      color17 #a3685a
      color18 #282a2e
      color19 #373b41
      color20 #b4b7b4
      color21 #e0e0e0
    '';
  };

  programs.git = {
    enable = true;
    userEmail = "sunilgunisetty@gmail.com";
    userName = "Sunil Gunisetty";

    ignores = [
      ".DS_Store"
    ];

    aliases = {
      st = "status";
      ci = "commit";
      lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --";
      co = "checkout";
      cane = "commit --amend --no-edit";
      dc = "diff --cached";
    };
    extraConfig = {
      core = {
        whitespace = "trailing-space,space-before-tab";
      };
      pull = {
        ff = "only";
      };
    };
  };

  home.file = {
    ".emacs.d" = {
      source = ./dotfiles/emacs;
      recursive = true;
    };
    ".git/hooks/commit-msg" = {
      text = ''
        #!/bin/sh

        COMMIT_FILE=$1
        COMMIT_MSG=$(cat $1)
        CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
        JIRA_ID=$(echo "$CURRENT_BRANCH" | grep -Eo "[A-Z0-9]{1,10}-?[A-Z0-9]+-\d+")
        if [ ! -z "$JIRA_ID" ]; then
            echo "[$JIRA_ID] $COMMIT_MSG" > $COMMIT_FILE
            echo "JIRA IS '$JIRA_ID' prepended to commit message. (Use --no-vefify to skip)"
        fi
      '';
    };
  };

  home.sessionVariables = {
    TERM="xterm-256color";
    EDITOR="emacs -nw";
    TERMINAL = "zsh";
    FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border";
    GOPATH="$HOME/go";
    PATH="$PATH:$GOPATH/bin";
  };
}
