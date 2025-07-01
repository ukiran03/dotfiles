function fish_prompt --description 'Write out the prompt'
    set -l last_status $status
    set -l user_host (set_color -o blue)
    set -l normal (set_color normal)
    set -l status_color (set_color -o brgreen)  # Bold bright green
    set -l cwd_color (set_color -o cyan -b black)
    set -l vcs_color (set_color -o white)  # Bold bright purple
    set -l prompt_status ""

    set -l login_color (set_color -o brblue)
    # set -lx fish_color_user $login_color  # Remove (set_color) here

    # Since we display the prompt on a new line allow the directory names to be longer.
    set -q fish_prompt_pwd_dir_length
    or set -lx fish_prompt_pwd_dir_length 0

    # Color the prompt differently when we're root
    set -l suffix '● $'
    if functions -q fish_is_root_user; and fish_is_root_user
        if set -q fish_color_cwd_root
            set cwd_color (set_color -o $fish_color_cwd_root)  # Bold root directory color
        end
        set suffix '#'
    end

    # Color the prompt in red on error
    if test $last_status -ne 0
        set status_color (set_color -o $fish_color_error)  # Bold error color
        set prompt_status $status_color "(" $last_status ")" $normal ' '
    end

    # Print the prompt
    echo -s $login_color $USER $normal @(prompt_hostname) $normal ' ' $cwd_color (prompt_pwd) $normal $vcs_color (fish_vcs_prompt) $normal ' '
    echo -n -s $prompt_status $status_color $suffix ' ' $normal
    # echo -n -s $prompt_status $status_color $normal (set_color red)'❯'(set_color yellow)'❯'(set_color green)'❯ '
end
