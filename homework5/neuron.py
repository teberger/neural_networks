__author__ = 'Taylor Berger'

class Neuron():
    def __init__(self, bias, activation_func, activation_prime):
        self.bias  = bias
        self.induced_field = 0.0
        self.output = 0.0
        self.local_gradient = 0.0
        self.phi = activation_func
        self.phi_prime = activation_prime
        self.inputs = {}
        self.outputs = []

    def addInput(self, input, weight):
        self.inputs[input] = weight

    def addOutput(self, o):
        self.outputs.append(o)

    def forward_prop(self):
        self.induced_field = 0.0
        for i in self.inputs.keys():
            self.induced_field += i.output * self.inputs[i]

        self.output = self.phi(self.induced_field + self.bias)

    def back_prop(self, eta, phi_prime):
        #first calculate my own local gradient for the neurons behind me
        self.local_gradient = 0
        for o in self.outputs:
            self.local_gradient += o.inputs[self] * o.local_gradient

        self.local_gradient *= self.phi_prime(self.induced_field)

        #adjust the input weights from me to the neurons I provide input to
        #based on the back propagation calculations
        for i in self.inputs:
            self.inputs[i] = self.inputs[i] + eta * i.local_gradient * self.output
