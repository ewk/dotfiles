# Anything that produces output must be guarded here
if status is-interactive
    set --universal fish_greeting
    keychain --eval --quiet >/dev/null  # only ask for password once per session
end

# Update $PATH
fish_add_path $HOME/.cargo/bin

if test -e $HOME/bin
    fish_add_path $HOME/bin
end

set --export --universal EDITOR nvim
set --export --universal SUDO_EDITOR nvim
set --export --universal CSCOPE_EDITOR nvim

abbr --add ll 'ls --all --human-readable --group-directories-first -l'
abbr --add grep 'grep -E --ignore-case'
abbr --add gpg gpg2

function open
    xdg-open $argv
end

fish_config theme choose coolbeans
