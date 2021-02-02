{ config, pkgs, ... }:

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
        doom-themes
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
    '';
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
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
  };
}
