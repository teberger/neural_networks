package unm.edu.tberge01.fall2014.cs547.generators;

import unm.edu.tberge01.fall2014.cs547.neurons.Neuron;

public interface NeuronGenerator {
	public <T extends Neuron> T generate();
}
