__author__ = 'Taylor'

import random

class NeuralNetwork():
    def __init__(self, seed, init_weights = 'random'):
        """

        :param seed:
        :param init_weights:
        """
        self.random = random.seed(seed)
        self.weights = init_weights
        self.layers = {}
        self.inputs = set()
        self.outputs = set()

    def randomizeWeight(self):
        return self.random.random()

    def addLayer(self, neurons, layer_number):
        self.layers[layer_number] = neurons

        if layer_number == 0:
            self.inputs = set(neurons)
            return

        for input in self.layers[layer_number - 1]:
            for output in neurons:
                output.addOutput(input)
                weight = self.randomizeWeight()
                input.addInput(output, weight)

