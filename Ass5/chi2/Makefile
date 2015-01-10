all:
	happy -gca Parchi.y
	alex -g Lexchi.x
	latex Docchi.tex
	dvips Docchi.dvi -o Docchi.ps
	ghc --make Testchi.hs -o Testchi
clean:
	 rm -f *.log *.aux *.hi *.o *.dvi Docchi.ps *.o
distclean: clean
	 rm -f Docchi.* Lexchi.* Parchi.* ParchiData.* Layoutchi.* Skelchi.* Printchi.* Testchi.* Abschi.* Testchi ErrM.* SharedString.* chi.dtd XMLchi.* Makefile*
