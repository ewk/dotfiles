# Anything that produces output must be guarded here
if status is-interactive
    set --universal fish_greeting
end

# Update $PATH
fish_add_path $HOME/.cargo/bin

if test -e $HOME/bin
    fish_add_path $HOME/bin
end

set --export --global EDITOR nvim
set --export --global SUDO_EDITOR nvim
set --export --global CSCOPE_EDITOR nvim

abbr --add ll 'ls --all --human-readable --group-directories-first -l'
abbr --add grep 'grep -E --ignore-case'
abbr --add gpg gpg2

function open
    xdg-open $argv
end

fish_config theme choose coolbeans
