let Enumerated = { index : Natural, value : Text }

let isIndex =
      λ(idx : Natural) →
      λ(n : Natural) →
            Natural/isZero (Natural/subtract n idx)
        &&  Natural/isZero (Natural/subtract idx n)

let getColorName =
      λ(idx : Natural) →
        if    Natural/isZero idx
        then  "black:   "
        else  if isIndex 1 idx
        then  "red:     "
        else  if isIndex 2 idx
        then  "green:   "
        else  if isIndex 3 idx
        then  "yellow:  "
        else  if isIndex 4 idx
        then  "blue:    "
        else  if isIndex 5 idx
        then  "magenta: "
        else  if isIndex 6 idx
        then  "cyan:    "
        else  if isIndex 7 idx
        then  "white:   "
        else  if isIndex 8 idx
        then  "black:   "
        else  if isIndex 9 idx
        then  "red:     "
        else  if isIndex 10 idx
        then  "green:   "
        else  if isIndex 11 idx
        then  "yellow:  "
        else  if isIndex 12 idx
        then  "blue:    "
        else  if isIndex 13 idx
        then  "magenta: "
        else  if isIndex 14 idx
        then  "cyan:    "
        else  if isIndex 15 idx
        then  "white:   "
        else  "black:   "

in  λ(theme : ./Theme) →
          ''
          colors:
            cursor:
              text:   '${theme.text}'
              cursor: '${theme.cursor}'
            primary:
              background: '${theme.background}'
              foreground: '${theme.foreground}'
            search:
              matches:
                foreground: '#000000'
                background: '#00ff00'
            normal:
          ''
      ++  List/fold
            Enumerated
            ( List/reverse
                { index : Natural, value : Text }
                (List/indexed Text theme.palette16)
            )
            Text
            ( λ(a : Enumerated) →
              λ(b : Text) →
                    b
                ++  ( if    isIndex 8 a.index
                      then  ''
                              bright:
                            ''
                      else  ""
                    )
                ++  "    "
                ++  getColorName a.index
                ++  ''
                    '${a.value}'
                    ''
            )
            ""
