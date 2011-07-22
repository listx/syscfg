FLAGS=-O2 -Wall -Werror
all: poke rouse usbmnt
poke: poke.hs
	ghc --make ${FLAGS} poke
rouse: rouse.hs
	ghc --make -threaded ${FLAGS} rouse
usbmnt: usbmnt.hs
	ghc --make ${FLAGS} usbmnt
