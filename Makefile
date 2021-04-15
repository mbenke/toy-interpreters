GHC=stack ghc --

all: buildTest
	./Test

buildTest: 
	$(GHC) -o Test --make Test.hs

tiny1-test:
	$(GHC) TestTiny1
	./TestTiny1

tiny2-test:
	$(GHC) TestTiny2
	./TestTiny2

clean:
	-find . -name \*.hi -delete
	-find . -name \*.o -delete
	-rm -f Test

distclean: clean
	-find . -name \*~ -delete
