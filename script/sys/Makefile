FLAGS=-O2 -Wall -Werror
all: auca poke time up usbmnt
auca: auca.hs
	ghc --make ${FLAGS} -threaded auca
poke: poke.hs
	ghc --make ${FLAGS} poke
time: time.hs
	ghc --make ${FLAGS} time
up: up.hs
	ghc --make -threaded ${FLAGS} up
usbmnt: usbmnt.hs
	ghc --make ${FLAGS} usbmnt
