__author__ = 'Taylor'

from numpy import *
from neuron import *
from neural_network import *
from functools import *
import sys


def useage():
    print '''
             Usage is as follows:
                arg1: random seed
                arg2: eta parameter
                argX: number of neurons in hidden layer X as an integer

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
    eta = float(args[2])
    layer_strings = args[3:]
    layers = []
    try:
        layers = [int(l) for l in layer_strings]
    except ValueError:
        useage()
        exit(1)

    return seed, eta, layers


#TODO: toy with the number of layers, neurons per layer, random seeding, etc
if __name__ == '__main__':
    seed, eta, layers = parseArgs(sys.argv)
    random.seed(seed)
    phi = partial(logistic_function, 2)
    phi_prime = partial(logistic_deriv, 2)
    network = NeuralNetwork(eta, init_weights='random')

    #input layer
    network.add_layer([Neuron(activation_func=phi, activation_prime=phi_prime),
                       Neuron(activation_func=phi, activation_prime=phi_prime)], 0)
    print "Input Layer@0 created"

    for layer_number, neuron_count in enumerate(layers):
        neurons = [ Neuron(activation_func=phi, activation_prime=phi_prime) for _ in range(neuron_count) ]
        network.add_layer(neurons, layer_number+1)
        print "Hidden Layer@", layer_number + 1, "created"

    #output layer
    network.add_layer([Neuron(activation_func=phi, activation_prime=phi_prime),
                       Neuron(activation_func=phi, activation_prime=phi_prime),
                       Neuron(activation_func=phi, activation_prime=phi_prime),
                       Neuron(activation_func=phi, activation_prime=phi_prime)], len(layers) + 1)
    print "Output Layer@", len(layers) + 1, "created"
    print

    outputs = network.get_output_layer()
    desired = {1 : {outputs[0]:1.0,
                    outputs[1]:0.0,
                    outputs[2]:0.0,
                    outputs[3]:0.0},

               2 : {outputs[0]:0.0,
                    outputs[1]:1.0,
                    outputs[2]:0.0,
                    outputs[3]:0.0},

               3 : {outputs[0]:0.0,
                    outputs[1]:0.0,
                    outputs[2]:1.0,
                    outputs[3]:0.0},

               4 : {outputs[0]:0.0,
                    outputs[1]:0.0,
                    outputs[2]:0.0,
                    outputs[3]:1.0}
              }

    input_file = open('TrainingData.txt')
    training_raw = input_file.read()
    input_file = open('TestingData.txt')
    testing_raw = input_file.read()

    training_lines = training_raw.split('\n')
    testing_lines = testing_raw.split('\n')
    #training_lines = filter(lambda x: (x.startswith('2')), training_lines)

    for  i in range(1000):
        random.shuffle(training_lines)
        error_epoch = 0
        classification_errors_epoch = 0
        classifications = {1:0, 2:0, 3:0, 4:0}

        for line in training_lines:
            #print line
            line_sep = line.split(' ')
            line_sep = filter(lambda x : x != '', line_sep)

            if(len(line_sep) != 4):
                continue

            c = int(line_sep[0])
            xs = [float(line_sep[2]), float(line_sep[3])]
            mapping = dict(zip(network.get_input_layer(), xs))
            network.forward_propagation(mapping)

            winner = 0
            z = 1
            output = -1.0
            for n in network.get_output_layer():
            #    print n.y_output
                if n.y_output > output:
                    output = n.y_output
                    winner = z
                z+=1
            classifications[winner] += 1
            if c != winner:
                classification_errors_epoch += 1

            error_epoch += network.backward_propagation(desired[c])



        classification_error_testing = 0
        testing_epoch_error = 0
        classifications_testing = {1:0, 2:0, 3:0, 4:0}

        for line in testing_lines:
            line_sep = line.split(' ')
            line_sep = filter(lambda x : x != '', line_sep)

            if(len(line_sep) != 4):
                continue

            c = int(line_sep[0])
            xs = [float(line_sep[2]), float(line_sep[3])]
            mapping = dict(zip(network.get_input_layer(), xs))
            network.forward_propagation(mapping)

            winner = 0
            z = 1
            output = -1.0
            for n in network.get_output_layer():
                e = desired[c][n] - n.y_output
                testing_epoch_error +=  e*e/2

                if n.y_output > output:
                    output = n.y_output
                    winner = z
                z+=1
            classifications_testing[winner] += 1
            if c != winner:
                classification_error_testing += 1

        print
        print
        print 'Epoch', i
        print 'Number of Incorrect Training Classifications:', classification_errors_epoch
        for c in classifications.keys():
            print 'Class', c, 'count:', classifications[c]
        print "Training Epoch Error:", error_epoch

        print

        print 'Number of Incorrect Testing Classifications', classification_error_testing
        for c in classifications_testing.keys():
            print 'Class', c, 'count:', classifications_testing[c]
        print 'Testing Epoch Error:', testing_epoch_error

    #TODO: Output charts, plots, generalization graphs & tables, etc.
