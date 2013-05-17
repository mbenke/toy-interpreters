BINFILES = testChecker1

all: $(BINFILES)

testChecker1: TestChecker1.hs Checker1.hs Syntax.hs
	ghc -o $@ --make TestChecker1 -main-is Rec1.TestChecker1.main -i..

clean:
	-rm -f *.hi *.o $(BINFILES)
