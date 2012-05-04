FLAGS=-O2 -Wall -Werror
all: auca poke panxor time up usbmnt
auca: auca.hs
	ghc --make ${FLAGS} -threaded auca
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
	$(RM) auca poke time up usbmnt
	find -type f -iregex ".+\.\(aux\|o\|hi\|hp\|mix\|ps\|tix\|html\)" -exec rm -v {} \;
