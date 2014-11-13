package unm.edu.tberge01.fall2014.cs547.networks;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Vector;

import unm.edu.tberge01.fall2014.cs547.neurons.InputNeuron;
import unm.edu.tberge01.fall2014.cs547.neurons.Neuron;
import unm.edu.tberge01.fall2014.cs547.neurons.SomNeuron;

public abstract class Lattice2d implements NeuralNetwork {
	
	protected InputNeuron[] inputs;
	protected SomNeuron[][] neurons;
	protected List<SomNeuron> neuron_list;
	
	
	public Lattice2d(int n, int m, InputNeuron[] inputs, Random weightGen) {
		
		this.inputs = inputs;
		this.neurons = new SomNeuron[n][m];
		this.neuron_list = new ArrayList<>(n*m);
		
		//construct the lattice
		for (int x = 0; x < n; x++)
			for (int y = 0; y < n; y++) {
				neurons[x][y] = new SomNeuron(x,y);
				neuron_list.add(neurons[x][y]);
			}
		
		//Hook up inputs
		for (Neuron[] row : neurons)
			for (Neuron neuron : row)
				for (Neuron i : inputs)
					neuron.addIncommingReference(i, weightGen.nextGaussian());
	}
	
	@Override
	public void setInputs(Vector<Double> inputValues) {
		assert(inputValues.size() == inputs.length);
		
		for(int i = 0; i < inputs.length; i++) {
			InputNeuron n = inputs[i];
			double val = inputValues.get(i);
			n.setInput(val);
		}
		
		for (SomNeuron n: this.neuron_list) 
			n.calculateLocalPotential();
	}
}
