{
  pkgs,
  config,
  ...
}: {
  xdg.configFile."nnn/plugins/fzcd".source = pkgs.writeShellScript "fzcd" ''

    # shellcheck disable=SC1090,SC1091
    . "$(dirname "$0")"/.nnn-plugin-helper

    CTX=+
    LIST="''${LIST:-""}"

    if ! type fzf >/dev/null 2>&1; then
        printf "fzf missing"
        read -r _
        exit 1
    fi

    if [ -n "$1" ] && [ "$(file -b --mime-type "$1")" = 'text/plain' ] && [ -e "$(head -1 "$1")" ]; then
        LIST="$1"
    elif ! [ -s "$LIST" ]; then
        sel=$(fzf)
        # Show only the file and parent dir
        # sel=$(fzf --delimiter / --with-nth=-2,-1 --tiebreak=begin --info=hidden)

        LIST='''
    fi

    if [ -n "$LIST" ]; then
        if type find >/dev/null 2>&1; then
            tmpfile=$(mktemp /tmp/abc-script.XXXXXX)

            while IFS= read -r path; do
                if [ -d "$path" ]; then
                    printf "%s\n" "$path" >> "$tmpfile"
                elif [ -f "$path" ]; then
                    printf "%s\n" "$(dirname "$path")" >> "$tmpfile"
                fi
            done < "$LIST"

            sel=$(xargs -d '\n' < "$tmpfile" -I{} find {} -type f -printf "%H//%P\n" | sed '/.*\/\/\(\..*\|.*\/\..*\)/d; s:/\+:/:g' | fzf --delimiter / --tiebreak=begin --info=hidden)
            # Alternative for 'fd'
            # sel=$(xargs -d '\n' < "$tmpfile" fd . | fzf --delimiter / --tiebreak=begin --info=hidden)

            rm -- "$tmpfile"
        else
            printf "find missing"
            read -r _
                exit 1
        fi
    fi

    if [ -n "$sel" ]; then
        if [ "$sel" = "." ] || { ! [ -d "$sel" ] && ! [ -f "$sel" ]; }; then
            exit 0
        fi

        # Check if the selected path returned by fzf command is absolute
        case $sel in
        /*) nnn_cd "$sel" "$CTX" ;;
        *)
            # Remove "./" prefix if it exists
            sel="''${sel#./}"

            if [ "$PWD" = "/" ]; then
                nnn_cd "/$sel" "$CTX"
            else
                nnn_cd "$PWD/$sel" "$CTX"
            fi;;
        esac
    fi
  '';
}
