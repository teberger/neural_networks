__author__ = 'Taylor Berger'

class Neuron():
    def __init__(self, bias, activation_func, activation_prime):
        self.bias  = bias
        self.induced_field = 0.0
        self.y_output = 0.0
        self.local_gradient = 0.0
        self.phi = activation_func
        self.phi_prime = activation_prime
        self.inputs = []
        self.outputs = {}

    def __str__(self):
        return 'Neuron: \n\tWeights: ' + str(self.outputs) + '\n'

    def __repr__(self):
        return str(self)

    def add_output_connection(self, output_node, weight):
        self.outputs[output_node] = weight

    def add_input_reference(self, i):
        self.inputs.append(i)

    def forward_prop(self):
        self.induced_field = 0.0

        for i in self.inputs:
            self.induced_field += i.outputs[self] * i.y_output

        self.induced_field += self.bias

        self.y_output = self.phi(self.induced_field)

    def back_prop(self, eta):
        #first calculate my own local gradient for the neurons behind me
        self.local_gradient = 0
        for o in self.outputs.keys():
            self.local_gradient += self.outputs[o] * o.local_gradient

        self.local_gradient *= self.phi_prime(self.induced_field)

        #adjust the input weights from me to the neurons I provide input to
        #based on the back propagation calculations
        for i in self.outputs.keys():
            self.outputs[i] += eta * i.local_gradient * self.y_output
