.PHONY : all clear

all : astar

astar : astar.hs
	ghc astar.hs

clean :
	rm -f astar.hi astar.o

