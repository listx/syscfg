let Mapped = { mapKey : Text, mapValue : Text }

in  λ(theme : ./Theme) →
      List/fold
        Mapped
        (List/reverse Mapped (./toColorMap.dhall theme))
        Text
        ( λ(a : Mapped) →
          λ(b : Text) →
                b
            ++  ''
                (setq l/color-${a.mapKey} "${a.mapValue}")
                ''
        )
        ""
