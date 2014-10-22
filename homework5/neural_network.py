__author__ = 'Taylor'

import random

class NeuralNetwork():
    def __init__(self, seed, init_weights = 'random', init_weight = 0):
        """

        :param seed:
        :param init_weights:
        """
        self.random = random.seed(seed)
        self.weight_style = init_weight_style
        self.init_weight = init_weight
        self.layers = {}
        self.inputs = set()
        self.outputs = set()

    def randomizeWeight(self):
        if self.weight_style == 'random':
            return self.random.random()
        else:
            return self.init_weight

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
