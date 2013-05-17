BINFILES = testChecker1

all: $(BINFILES)

testChecker1:
	ghc -o $@ --make TestChecker1 -main-is Rec1.TestChecker1.main -i..

clean:
	-rm -f *.hi *.o $(BINFILES)
