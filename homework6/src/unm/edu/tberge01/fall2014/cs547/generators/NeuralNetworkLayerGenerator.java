package unm.edu.tberge01.fall2014.cs547.generators;

import java.util.HashSet;
import java.util.Set;

import unm.edu.tberge01.fall2014.cs547.neurons.Neuron;

public class NeuralNetworkLayerGenerator {
	
	private final int[] layerCounts;
	private final NeuronGenerator[] layerGenerator;
	
	public NeuralNetworkLayerGenerator(int[] layerCounts, NeuronGenerator[] layer_generator) {
		assert(layerCounts.length == layer_generator.length);
		
		this.layerCounts = layerCounts;
		this.layerGenerator = layer_generator;
	}
	
	public Set<Neuron>[] generateLayers() {
		@SuppressWarnings("unchecked")
		Set<Neuron>[] layers = new Set[layerCounts.length];
		
		for (int i = 0; i < layerCounts.length; i++) {
			int count = layerCounts[i];
			NeuronGenerator gen = layerGenerator[i];
			Set<Neuron> neurons = new HashSet<>();
			
			for(int j = 0; j < count; j++)
				neurons.add(gen.generate());
			
			layers[i] = neurons;
		}
		
		return layers;
	}
}
