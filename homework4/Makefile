default: compile

compile: 
	ghc perceptron.hs -o perceptron

clean: 
	rm *.hi; rm perceptron; rm *.o; rm *~

run: compile
	./perceptron PercepClass1Training.txt PercepClass2Training.txt PDRClass1Testing.txt PDRClass2Testing.txt
