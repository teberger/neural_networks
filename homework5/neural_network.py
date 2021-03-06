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
        self.e_squared = 0
        self.layers = {}

    def randomize_weight(self):
        """

        :return:
        """
        if self.weight_style == 'random':
            return (random.random())
        else:
            return self.init_weight

    def add_layer(self, neurons, layer_number):
        """

        :param neurons:
        :param layer_number:
        :return:
        """
        bias_value = random.randint(1,10)
        bias = Neuron(activation_func=lambda x: 0, activation_prime=lambda x: 0, isBias=True)
        bias.y_output = bias_value

        neurons.append(bias)
        self.layers[layer_number] = neurons

        if layer_number == 0:
            return

        if layer_number > self.max_layer:
            self.max_layer = layer_number

        for input in self.layers[layer_number - 1]:
            for output in neurons:
                output.add_input_reference(input)

                if not output.isBias:
                    weight = self.randomize_weight()
                    input.add_output_connection(output, weight)


    def forward_propagation(self, input_mapping):
        for n in self.get_input_layer():
            n.y_output = input_mapping[n]

        #excluding the input layer, iterate through and propagate
        #information forward

        for i in range(1, len(self.layers)):
#            print 'layer', i
            for neuron in self.layers[i]:
                neuron.forward_prop()
#        exit()

    def backward_propagation(self, desired_output_mapping):
        self.e_squared = 0
        for n in self.get_output_layer():
            error = desired_output_mapping[n] - n.y_output
            self.e_squared += error * error / 2
            n.local_gradient = error * n.phi_prime(n.induced_field)

        #iterate backward through the layers to the input layer, excluding the output
        for i in range(len(self.layers)-2, -1, -1):
            for neuron in self.layers[i]:
                neuron.back_prop(self.eta)

        return self.e_squared

    def get_input_layer(self):
        return filter(lambda x: not x.isBias, self.layers[0])

    def get_output_layer(self):
        return filter(lambda x: not x.isBias, self.layers[self.max_layer])