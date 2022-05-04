let Enumerated = { index : Natural, value : Text }

let Mapped = { mapKey : Text, mapValue : Text }

let isIndex =
      λ(idx : Natural) →
      λ(n : Natural) →
            Natural/isZero (Natural/subtract n idx)
        &&  Natural/isZero (Natural/subtract idx n)

let getColorName =
      λ(idx : Natural) →
        if    Natural/isZero idx
        then  "black"
        else  if isIndex 1 idx
        then  "red"
        else  if isIndex 2 idx
        then  "green"
        else  if isIndex 3 idx
        then  "yellow"
        else  if isIndex 4 idx
        then  "blue"
        else  if isIndex 5 idx
        then  "magenta"
        else  if isIndex 6 idx
        then  "cyan"
        else  if isIndex 7 idx
        then  "white"
        else  if isIndex 8 idx
        then  "brightblack"
        else  if isIndex 9 idx
        then  "brightred"
        else  if isIndex 10 idx
        then  "brightgreen"
        else  if isIndex 11 idx
        then  "brightyellow"
        else  if isIndex 12 idx
        then  "brightblue"
        else  if isIndex 13 idx
        then  "brightmagenta"
        else  if isIndex 14 idx
        then  "brightcyan"
        else  if isIndex 15 idx
        then  "brightwhite"
        else  "?"

in  λ(theme : ./Theme) →
        [ { mapKey = "text", mapValue = theme.text }
        , { mapKey = "cursor", mapValue = theme.cursor }
        , { mapKey = "background", mapValue = theme.background }
        , { mapKey = "foreground", mapValue = theme.foreground }
        ]
      # List/fold
          Enumerated
          (List/reverse Enumerated (List/indexed Text theme.palette16))
          (List Mapped)
          ( λ(a : Enumerated) →
            λ(b : List Mapped) →
              b # [ { mapKey = getColorName a.index, mapValue = a.value } ]
          )
          ([] : List Mapped)
      # toMap theme.paletteExtra
