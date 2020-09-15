all: test

.PHONY: test clean

test: tester
	@echo "Printing test results to 'test-results.txt'"
	@./tester > test-results.txt
	@echo "Tests completed."

tester: Main.hs Tests.hs P0.hs
	ghc -o tester Main.hs

clean:
	rm -rf tester test-results.txt *.hi *.o

