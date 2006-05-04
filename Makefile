RUNHS=runhaskell

help:
	@echo ""
	@echo "This Makefile will assist you to compile Edison.  The Edison source distribution is broken into a number of Cabal sub-packages, some of which depend on others.  If you prefer, you may invoke the cabal build system for the sub-packages manually.  This will allow you to have greater control over exactily which packages are installed in which places."
	@echo ""
	@echo "To install all Edison packages into your home directory, type 'make user'."
	@echo ""
	@echo "To install all Edison packages in the system-wide package repository, type 'make system'.  This may require superuser privliges."
	@echo ""
	@echo "To run the unit tests, type 'make test'.  This will install Edison into your home directory and run the unit tests."
	@echo "See the README file for more details."
	@echo ""

user : api-user core-user
system : api-system core-system
clean : api-clean core-clean test-clean

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
