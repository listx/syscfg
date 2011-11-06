FLAGS=-O2 -Wall -Werror
all: auca poke up usbmnt
auca: auca.hs
	ghc --make ${FLAGS} -threaded auca
poke: poke.hs
	ghc --make ${FLAGS} poke
up: up.hs
	ghc --make -threaded ${FLAGS} up
usbmnt: usbmnt.hs
	ghc --make ${FLAGS} usbmnt
