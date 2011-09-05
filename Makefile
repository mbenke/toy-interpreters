all: buildTest
	./Test

buildTest: 
	ghc -o Test --make Test.hs

clean:
	-find . -name *.hi -o -name *.o -print | xargs rm -f
	-rm -f Test

distclean: clean
	-find . -name *~ -print | xargs rm -f