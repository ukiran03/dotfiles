#!/usr/bin/env bash

pac_qqe=$(pacman -Qqe)
pac_qqm=$(pacman -Qqm)
pac_qq=$(pacman -Qq)

exp_pkgs=$(echo "$pac_qqe" | wc -l)
all_pkgs=$(echo "$pac_qq" | wc -l)
for_pkgs=$(echo "$pac_qqm" | wc -l)

out_str="Last Updated: $(date)\n\nTotal Packages: $all_pkgs\nForeign Packages: $for_pkgs\n\n"
count=1
for pkg in $pac_qqe
do
    out_str="$out_str$pkg"
    
    for for_pkg in $pac_qqm
    do
        if [[ $pkg == $for_pkg ]]
        then
            out_str="$out_str (FOREIGN)"
        fi
    done
    
    if [[ $count -eq $exp_pkgs ]]
    then
        break   
    fi

    count=$((count+1))
    out_str="$out_str\n"
done

echo -e $out_str
