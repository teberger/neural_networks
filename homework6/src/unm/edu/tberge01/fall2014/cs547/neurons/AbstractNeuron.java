package unm.edu.tberge01.fall2014.cs547.neurons;

import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;

public abstract class AbstractNeuron implements Neuron {
	
	protected double output;
	protected final SortedMap<Neuron, Double> inputConnections;
	protected final Set<Neuron> outputConnections;
	
	public AbstractNeuron(SortedMap<Neuron, Double> inputs, Set<Neuron> outputs) {
		this.inputConnections = inputs;
		this.outputConnections = outputs;
		this.output = 0.0;
	}
	
	@Override
	public double getOutput() {
		return this.output;
	}
	
	/**
	 * Adds the parameter 'value' to the current weight of the
	 * connection
	 *
	 */
	@Override
	public boolean adjustWeight(Neuron n, double value) {
		if (this.inputConnections.containsKey(n)) {
			double prev = this.inputConnections.get(n);
			this.inputConnections.put(n, prev + value);
			
			return true;
		}
		return false;
	}
		

	@Override
	public Map<Neuron, Double> incommingReferences() {
		return Collections.unmodifiableMap(this.inputConnections);
	}

	@Override
	public Set<Neuron> outgoingReferences() {
		return Collections.unmodifiableSet(this.outputConnections);
	}

	@Override
	public boolean addOutgoingReference(Neuron n) {
		return this.outputConnections.add(n);
	}

	@Override
	public boolean addIncommingReference(Neuron n, double init_weight) {
		
		if (inputConnections.keySet().contains(n))
			return false;
		
		inputConnections.put(n, init_weight);
		return true;
	}

	@Override
	public int compareTo(Neuron n) {
		return this.hashCode() - n.hashCode();
	}
}
