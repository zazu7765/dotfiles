function unstowdots
    # Default dotfiles location
    set -l dotfiles_dir ~/dotfiles

    # Check if dotfiles directory is specified
    if test (count $argv) -ge 3; and test "$argv[1]" = "-d"
        set dotfiles_dir $argv[2]
        set -e argv[1]
        set -e argv[1]
    end

    switch $argv[1]
        case 'config'
            cd $dotfiles_dir/config
            stow -D -t ~/.config --verbose $argv[2..-1]
        case 'home'
            cd $dotfiles_dir/home
            stow -D -t ~ --verbose $argv[2..-1]
        case 'all'
            cd $dotfiles_dir/config
            stow -D -t ~/.config --verbose *
            cd $dotfiles_dir/home
            stow -D -t ~ --verbose *
        case '*'
            echo "Usage: unstowdots [-d DOTFILES_DIR] [config|home|all] [package1 package2 ...]"
            echo "  Default DOTFILES_DIR is ~/dotfiles"
    end
end
