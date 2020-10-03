let Enumerated = { index : Natural, value : Text }

in  λ(theme : ./Theme) →
          ''
          # Foreground color. (Default text color).
          printf '\x1b]10;${theme.foreground}\x07'

          # Background color.
          printf '\x1b]11;${theme.background}\x07'

          # Cursor color.
          printf '\x1b]12;${theme.cursor}\x07'

          # Remaining 16 colors.
          ''
      ++  List/fold
            Enumerated
            (List/indexed Text theme.palette16)
            Text
            ( λ(a : Enumerated) →
              λ(b : Text) →
                    b
                ++  ''
                    printf '\x1b]4;${Natural/show a.index};${a.value}\x07'
                    ''
            )
            ""
