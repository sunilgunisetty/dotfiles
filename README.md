# dotfiles
Using home-manager to configure development environment

# Installation
1. [Install Nix](https://nixos.org/download.html)

2. [Install Home Manager](https://github.com/nix-community/home-manager#installation)

3. Run the following commangs
- clone `git clone git@github.com:sunilgunisetty/dotfiles.git nixpkgs`
- remove existing configurations, if you don't have any previous configuration ignore this step. `rm -rf ~/.config/nixpkgs`
- move files to config `mv nixpkgs ~/.config/`

4. To activate `home-manager switch`
