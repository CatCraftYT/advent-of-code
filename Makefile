all:
	find . -name "*.hs" | parallel "ghc -dynamic -no-keep-hi-files -no-keep-o-files -o {.}.out {}"