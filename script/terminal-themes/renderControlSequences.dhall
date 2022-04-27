let Enumerated = { index : Natural, value : Text }

let controlSequence =
      λ(forTmux : Bool) →
        if    forTmux
        then  { start = "\\x1bPtmux;\\x1b\\x1b]", end = "\\x07\\x1b\\\\" }
        else  { start = "\\x1b]", end = "\\x07" }

let setColor =
      λ(forTmux : Bool) →
      λ(code : Natural) →
      λ(color : Text) →
        ''
        printf '${    (controlSequence forTmux).start
                  ++  Natural/show code
                  ++  ";"
                  ++  color
                  ++  (controlSequence forTmux).end}'
        ''

in  λ(forTmux : Bool) →
    λ(theme : ./Theme) →
          ''
          # Foreground color. (Default text color).
          ${setColor forTmux 10 theme.foreground}

          # Background color.
          ${setColor forTmux 11 theme.background}

          # Cursor background color.
          ${setColor forTmux 12 theme.cursor}
          ''
      ++  ( if    forTmux
            then  ''
                  # Set colors for TMUX status bar. We just recycle the colors defined
                  # for the cursor.
                  tmux set-environment L_TMUX_STATUS_BG "${theme.text}"
                  tmux set-environment L_TMUX_STATUS_FG "${theme.cursor}"
                  tmux source-file ~/.tmux/colors.conf

                  ''
            else  ''
                  export L_TMUX_STATUS_BG="${theme.text}"
                  export L_TMUX_STATUS_FG="${theme.cursor}"
                  ''
          )
      ++  ''
          # Remaining 16 colors.
          ''
      ++  List/fold
            Enumerated
            (List/indexed Text theme.palette16)
            Text
            ( λ(a : Enumerated) →
              λ(b : Text) →
                b ++ setColor forTmux 4 "${Natural/show a.index};${a.value}"
            )
            ""
