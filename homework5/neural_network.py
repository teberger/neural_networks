__author__ = 'Taylor'

import random
from neuron import Neuron

class NeuralNetwork():
    def __init__(self, eta, init_weights = 'random', init_weight = 0):
        """

        :param seed:
        :param init_weights:
        :param init_weight:
        """
        self.eta = eta
        self.weight_style = init_weights
        self.init_weight = init_weight
        self.max_layer = 0
        self.layers = {}

    def randomize_weight(self):
        """

        :return:
        """
        if self.weight_style == 'random':
            return random.random()
        else:
            return self.init_weight

    def add_layer(self, neurons, layer_number):
        """

        :param neurons:
        :param layer_number:
        :return:
        """
        self.layers[layer_number] = neurons

        if layer_number == 0:
            return

        if layer_number > self.max_layer:
            self.max_layer = layer_number

        for input in self.layers[layer_number - 1]:
            for output in neurons:
                output.add_input_reference(input)
                weight = self.randomize_weight()
                input.add_output_connection(output, weight)

    def forward_propagation(self, input_mapping):
        for n in self.get_input_layer():
            n.y_output = input_mapping[n]

        #excluding the input layer, iterate through and propagate
        #information forward
        for i in range(1, len(self.layers)):
            for neuron in self.layers[i]:
                neuron.forward_prop()

    def backward_propagation(self, desired_output_mapping):
        for n in self.get_output_layer():
            error = desired_output_mapping[n] - n.y_output
            n.local_gradient = error * n.phi_prime(n.induced_field)

        #iterate backward through the layers to the input layer.
        for i in range(len(self.layers)-1, -1, -1):
            #TODO: bias updating needs to be done here
            for neuron in self.layers[i]:
                neuron.back_prop(self.eta)

    def get_input_layer(self):
        return self.layers[0]

    def get_output_layer(self):
        return self.layers[self.max_layer]