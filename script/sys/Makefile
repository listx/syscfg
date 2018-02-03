FLAGS=-O2 -Wall -Werror
poke: poke.hs
	ghc --make ${FLAGS} poke
panxor: panxor.hs
	ghc --make ${FLAGS} panxor
time: time.hs
	ghc --make ${FLAGS} time
up: up.hs
	ghc --make -threaded ${FLAGS} up
usbmnt: usbmnt.hs
	ghc --make ${FLAGS} usbmnt
clean:
	$(RM) poke time up usbmnt
	find -type f -iregex ".+\.\(aux\|o\|hi\|hp\|mix\|ps\|tix\|html\)" -exec rm -v {} \;
