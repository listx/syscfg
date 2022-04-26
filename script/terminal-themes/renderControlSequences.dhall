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
