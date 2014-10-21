__author__ = 'Taylor'

from neuron import *
import sys
from numpy import *

def logistic_function(a, x):
    return 1 / (1 + exp(a*x))

if __name__ == '__main__':
    layers = sys.argv[1:]

    network = NeuralNetwork('random_weights', 'full-connectivity')
    for layer_number,neuron_count in enumerate(layers):
        neurons = []
        for i in range(neuron_count):
            neurons.append(Neuron())

        network.addLayer(layer_number)

