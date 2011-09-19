all: buildTest
	./Test

buildTest: 
	ghc -o Test --make Test.hs

clean:
	-find . -name \*.hi -delete
	-find . -name \*.o -delete
	-rm -f Test

distclean: clean
	-find . -name \*~ -delete