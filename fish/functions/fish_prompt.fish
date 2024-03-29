function fish_prompt --description "minimal VCS prompt"
    set -l _display_status $status

    set -g __fish_git_prompt_showcolorhints 1
    set -g __fish_git_prompt_showstashstate 1
    set -g __fish_git_prompt_showdirtystate 1
    set -g __fish_git_prompt_describe_style default
    set -g __fish_git_prompt_char_stashstate \U1F95E

    set -g fish_color_cwd yellow
    set -g fish_color_user normal
    set -g prompt_symbol \U276F

    printf '%s ' (prompt_login)
    set_color $fish_color_cwd

    printf '%s' (prompt_pwd)
    set_color normal

    printf '%s ' (__fish_git_prompt)

    if test $_display_status -ne 0
        set_color -o red
        printf '%s' $_display_status
        set_color normal
    end

    printf "%s " $prompt_symbol
end
