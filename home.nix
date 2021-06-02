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
    nodePackages.typescript
    nodePackages.prettier
    nodePackages.typescript-language-server
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
        kubernetes
        markdown-mode
        yaml-mode
        terraform-mode
        speed-type
        ample-theme
        prettier-js
        json-mode
        rjsx-mode
        tide
        haskell-mode
        dante
        humanoid-themes
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
      sshadd = "ssh-add ~/.ssh/svg_rsa";
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
      font_size = "14";
      disable_ligatures = "never";
      cursor_shape = "block";
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
      # Base16 3024 - kitty color config
      # Scheme by Jan T. Sott (http://github.com/idleberg)
      background #090300
      foreground #a5a2a2
      selection_background #a5a2a2
      selection_foreground #090300
      url_color #807d7c
      cursor #a5a2a2
      active_border_color #5c5855
      inactive_border_color #3a3432
      active_tab_background #991229
      active_tab_foreground #807d7c
      inactive_tab_background #3a3432
      inactive_tab_foreground #807d7c
      tab_bar_background #3a3432

      # normal
      color0 #090300
      color1 #db2d20
      color2 #01a252
      color3 #fded02
      color4 #01a0e4
      color5 #a16a94
      color6 #b5e4f4
      color7 #a5a2a2

      # bright
      color8 #5c5855
      color9 #db2d20
      color10 #01a252
      color11 #fded02
      color12 #01a0e4
      color13 #a16a94
      color14 #b5e4f4
      color15 #f7f7f7

      # extended base16 colors
      color16 #e8bbd0
      color17 #cdab53
      color18 #3a3432
      color19 #4a4543
      color20 #807d7c
      color21 #d6d5d4
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
    ".aws/config" = {
      text = ''
        [default]
        region=us-west-2
        output=json
        cli_auto_prompt = on-partial
        cli_pager=
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
