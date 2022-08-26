function fish_prompt --description "minimal VCS prompt"
    set -g __fish_git_prompt_showcolorhints 1
    set -g __fish_git_prompt_showstashstate 1
    set -g __fish_git_prompt_char_stashstate \U1F95E

    set -g fish_color_cwd yellow
    set -g fish_color_user normal

    printf '%s ' (prompt_login)
    set_color $fish_color_cwd

    printf '%s' (prompt_pwd)
    set_color normal

    printf '%s \U276F ' (__fish_git_prompt)
    set_color normal
end
