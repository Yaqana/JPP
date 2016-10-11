all:
	happy -gca Parmich.y
	alex -g Lexmich.x
	ghc --make Testmich.hs -o Testmich

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Docmich.ps

distclean: clean
	-rm -f Docmich.* Lexmich.* Parmich.* Layoutmich.* Skelmich.* Printmich.* Testmich.* Absmich.* Testmich ErrM.* SharedString.* ComposOp.* mich.dtd XMLmich.* Makefile*
	

