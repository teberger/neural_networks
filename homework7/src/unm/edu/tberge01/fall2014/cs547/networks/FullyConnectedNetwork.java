package unm.edu.tberge01.fall2014.cs547.networks;

import java.util.Set;
import java.util.Vector;

import unm.edu.tberge01.fall2014.cs547.neurons.InputNeuron;
import unm.edu.tberge01.fall2014.cs547.neurons.Neuron;

public abstract class FullyConnectedNetwork implements NeuralNetwork {

	protected final Vector<InputNeuron> inputs;
	protected final Set<Neuron>[] layers;
	protected final Vector<Neuron> outputs;
	
	public FullyConnectedNetwork(int inputSize, Set<Neuron>[] layers) {
		this.layers = layers;
		this.inputs = new Vector<>(inputSize);
		this.outputs = new Vector<>(layers[layers.length - 1]);
		
		//Set up inputs and connect them to the first layer in layers
		for(int i = 0; i < inputSize; i++)
			inputs.addElement(new InputNeuron());
		
		for (Neuron n : layers[0]) {
			for (Neuron inputNeuron : this.inputs) {
				inputNeuron.addOutgoingReference(n);
				n.addIncommingReference(inputNeuron, 1.0); //TODO: Choose this?
			}
		}
		
		//Add references to make each layer fully connected. 
		for (int i = 1; i < layers.length;  i++) {
			for (Neuron n : layers[i]) {
				for (Neuron inputNeuron : layers[i-1]) {
					inputNeuron.addOutgoingReference(n);
					n.addIncommingReference(inputNeuron, 1.0); //TODO: Choose this?
				}
			}
		}
	}

	@Override
	public Vector<Double> classify(Vector<Double> inputVector) {
		setInputs(inputVector);
		
		for (Set<Neuron> layer : this.layers) 
			for(Neuron n : layer) 
				n.calculateLocalPotential();
		
		Vector<Double> outputVector = new Vector<Double>(outputs.size());
		
		for (int i = 0; i < this.outputs.size(); i++) 
			outputVector.addElement(this.outputs.get(i).getOutput());
		
		return outputVector;
	}
	
	@Override
	public void setInputs(Vector<Double> inputs) {
		//set all input neuron's output to the given value
		for (int i = 0; i < inputs.size(); i++) {
			InputNeuron n = this.inputs.get(i);
			double val = inputs.get(i);
			n.setInput(val);
		}
	}
}
