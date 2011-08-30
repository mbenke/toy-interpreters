all: buildTest
	./Test

buildTest: 
	ghc -o Test --make Test.hs

clean:
	-rm -f *.{hi,o}
	-rm -f Test

distclean: clean
	-rm -rf *~