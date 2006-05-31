RUNHS=runhaskell

help:
	@echo ""
	@echo "****************************************************************"
	@echo "****************************************************************"
	@echo "*** Edison 1.2   Makefile help"
	@echo ""
	@echo "This Makefile will assist you to compile Edison.  The Edison source"
	@echo "distribution is broken into a number of Cabal sub-packages, some of which"
	@echo "depend on others.  If you prefer, you may invoke the cabal build system for"
	@echo "the sub-packages manually.  This will allow you to have greater control over"
	@echo "exactily which packages are installed in which places."
	@echo ""
	@echo "To install all Edison packages into your home directory, type 'make user'."
	@echo ""
	@echo "To install all Edison packages in the system-wide package repository,"
	@echo "type 'make system'.  This may require superuser privliges."
	@echo ""
	@echo "To run the unit tests, type 'make test'.  This will install Edison into your"
	@echo "home directory and run the unit tests."
	@echo ""
	@echo "See the README file for more details."
	@echo ""
	@echo "To collect all source files together for use in Hugs, type 'make hugs'."
	@echo "This will create a directory named 'hugs' which will contain the source code"
	@echo "from all the subprojects.  You can then copy this directory to an appropriate"
	@echo "place.  Call hugs with your package search path (the -P option) set to contain"
	@echo "this directory. Also be sure to use the -98 option when calling Hugs to enable"
	@echo "extensions."
	@echo ""
	@echo "To generate the API documentation, type 'make docs'.  This will generate Haddock"
	@echo "documentation in the 'dist/doc/html' directory."

user : api-user core-user
system : api-system core-system
clean : api-clean core-clean test-clean
	- rm -r hugs/

api-system :
	cd edison-api && \
          $(RUNHS) Setup.hs configure && \
          $(RUNHS) Setup.hs build && \
          $(RUNHS) Setup.hs install

api-user :
	cd edison-api && \
          $(RUNHS) Setup.hs configure --prefix=$(HOME) --user && \
          $(RUNHS) Setup.hs build && \
          $(RUNHS) Setup.hs install --user

api-clean :
	- cd edison-api && $(RUNHS) Setup.hs clean

core-system : api-system
	cd edison-core && \
          $(RUNHS) Setup.hs configure && \
          $(RUNHS) Setup.hs build && \
          $(RUNHS) Setup.hs install

core-user : api-user
	cd edison-core && \
          $(RUNHS) Setup.hs configure --prefix=$(HOME) --user && \
          $(RUNHS) Setup.hs build && \
          $(RUNHS) Setup.hs install --user

core-clean :
	- cd edison-core && $(RUNHS) Setup.hs clean

test : api-user core-user
	cd test && \
          $(RUNHS) Setup.hs configure --prefix=$(HOME) --user && \
          $(RUNHS) Setup.hs build && \
          ./dist/build/testSuite/testSuite

test-clean :
	- cd test && $(RUNHS) Setup.hs clean

hugs :
	mkdir hugs/
	cp -r edison-api/src/* hugs/
	cp -r edison-core/src/* hugs/

docs :
	mkdir -p dist/doc/html
	find edison-api/src edison-core/src -name '*.hs' -or -name '*.lhs' | xargs haddock -o dist/doc/html -h -t Edison
