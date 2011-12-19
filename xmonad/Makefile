HOST := $(shell hostname)
ifeq ('${HOST}', 'k2')
	NAME = xmonad-i386-linux
else
	NAME = xmonad-x86_64-linux
endif

all:
	ghc -O2 -Wall -Werror --make xmonad.hs -o ${NAME}

clean:
	$(RM) -f *.hi *.o ${NAME}
