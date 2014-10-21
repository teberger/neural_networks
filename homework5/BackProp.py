__author__ = 'Taylor'

from numpy import *
from neuron import *
import sys


def logistic_function(a, x):
    """
    The logistic function as defined in the book

    :param a : the slope parameter of the exponential
    :param x : the input value to calculate

    :rtype : a floating point value
    """
    return 1 / (1 + exp(a*x))

if __name__ == '__main__':
    layers = sys.argv[1:]

    #TODO: toy with the number of layers, neurons per layer, random seeding,

    network = NeuralNetwork('random_weights', 'full-connectivity')

    phi = logistic_function(1)

    for layer_number,neuron_count in enumerate(layers):
        neurons = []
        for i in range(neuron_count):
            neurons.append(Neuron())

        network.addLayer(layer_number)

    #TODO: Setup output and input layers
    #TODO: Read input and map to input layer, error correction
    #TODO: Output charts, plots, generalization graphs & tables, etc.
