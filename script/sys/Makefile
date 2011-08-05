FLAGS=-O2 -Wall -Werror
all: poke up usbmnt
poke: poke.hs
	ghc --make ${FLAGS} poke
up: up.hs
	ghc --make -threaded ${FLAGS} up
usbmnt: usbmnt.hs
	ghc --make ${FLAGS} usbmnt
