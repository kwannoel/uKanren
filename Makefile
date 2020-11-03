slides:
	pandoc -t revealjs -s -o index.html slides.md -V revealjs-url=https://unpkg.com/reveal.js@3.9.2/

.PHONY: tests
tests:
	runghc microKanren.hs
