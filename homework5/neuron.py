__author__ = 'Taylor Berger'

class Neuron():
    def __init__(self, bias, activation_func):
        self.bias  = bias
        self.output = 0.0
        self.error = 0.0
        self.phi = activation_func
        self.inputs = {}
        self.outputs = []

    def addInput(self, input, weight):
        self.inputs[input] = weight

    def addOutput(self, o):
        self.outputs.append(o)

    def forward_prop(self):
        induced_field = 0
        for i in self.inputs.keys():
            induced_field += i.output * self.inputs[i]

        self.output = self.phi(induced_field + self.bias)

    def back_prop(self):
        delta = 0
        for i in self.outputs:
            delta += i.inputs[self] * i.error

        #TODO: finish back propagation