
HAPPY 	= happy -gca
ALEX	= alex -g
GHC		= ghc
# to build with ghc installed by stack:
# GHC	= stack ghc --

generatedHS = Lexer.hs Parser.hs
exe = rubyc

.PHONY : all
all : $(exe)

.PHONY : demo
demo: $(exe)
				@for f in test/*.rb; do \
				    echo "Test $${f}:"; \
				    ./$(exe) $$f; echo ""; \
				 done


.PHONY : clean
clean:
				-rm -fv *.hi *.o *.hi-boot *.o-boot *.dyn_*
				@cd Compiler && rm -fv *.hi *.o *.hi-boot *.o-boot *.dyn_*
				@cd ..

.PHONY : cleanhaskell
cleanhaskell:
				-rm -fv $(generatedHS)

.PHONY : distclean
distclean: clean cleanhaskell
				-rm -fv rubyc

%.hs: %.y
				$(HAPPY) $<

%.hs: %.x
				$(ALEX) $<

$(exe): Main.hs Lexer.hs Parser.hs
				$(GHC) --make $< -o $@
