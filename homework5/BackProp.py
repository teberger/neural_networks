__author__ = 'Taylor'

from numpy import *
from neuron import *
from neural_network import *
import sys

def useage():
    print '''
             Usage is as follows:
                arg1: random seed
                argX: number of neurons in layer X as an integer

             the program will create as many hidden layers as there are
             arguments greater than 2
          '''

def logistic_function(a, x):
    """
    The logistic function as defined in the book

    :param a : the slope parameter of the exponential
    :param x : the input value to calculate

    :rtype : a floating point value
    """
    return 1 / (1 + exp(a*x))

def logistic_deriv(a, x):
    """
    The logistic function derivative as defined in the book

    :param a : the slope parameter of the exponential
    :param x : the input value to calculate

    :rtype : a floating point value
    """
    return a * logistic_function(a,x) * (1 - logistic_function(a, x))

def parseArgs(args):
    """
    Parses and returns the program arguments. If for any reason
    the parsing fails or an incorrect number of arguments were
    found, this function will print the usage doc and exit(1)

    :param args: the system arguments to parse
    :return: seed, layers. The seed is the rng seed and layers
                           is a list of integers determining
                           how many neurons to put in each layer
    """
    if len(args) <= 2:
        useage()
        exit(1)

    seed = int(args[1])
    layer_strings = args[2:]
    layers = []
    try:
        layers = [int(l) for l in layer_strings]
    except ValueError:
        useage()
        exit(1)

    return seed, layers


#TODO: toy with the number of layers, neurons per layer, random seeding,
if __name__ == '__main__':
    seed, layers = parseArgs(sys.argv)
    phi = logistic_function(1)
    phi_prime = logistic_deriv(1)
    network = NeuralNetwork(seed, init_weights='random_weights')

    for layer_number, neuron_count in enumerate(layers):
        #select some random bias for this layer
        bias = random.randint(-5,5)
        neurons = [ Neuron(bias, activation_func=phi, activation_prime=phi_prime) for _ in range(neuron_count) ]
        network.addLayer(neurons, layer_number)

    #TODO: Read input and map to input layer, error correction
    #TODO: Output charts, plots, generalization graphs & tables, etc.
