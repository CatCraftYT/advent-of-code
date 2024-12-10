all:
	find . -name "*.hs" | parallel "ghc -dynamic -threaded -no-keep-hi-files -no-keep-o-files -o {.}.out {}"

clean:
	rm *.out