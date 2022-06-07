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

abbr --add ls 'ls --color=auto'
abbr --add ll 'ls --all --human-readable --group-directories-first -l'
abbr --add grep 'egrep --color --ignore-case'
abbr --add gpg gpg2

function open
    command xdg-open $argv[1]
end

function on_exit --on-event fish_exit
    clear
end
