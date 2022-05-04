let Mapped = { mapKey : Text, mapValue : Text }

in  λ(theme : ./Theme) →
      List/fold
        Mapped
        (./toColorMap.dhall theme)
        Text
        ( λ(a : Mapped) →
          λ(b : Text) →
                b
            ++  ''
                ${a.mapKey}=${a.mapValue}
                ''
        )
        ""
