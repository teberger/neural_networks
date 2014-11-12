package unm.edu.tberge01.fall2014.cs547.neurons;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;


public class BiasNeuron implements Neuron {
	
	protected double bias_value;
	protected final Set<Neuron> outgoingConnections;
	
	public BiasNeuron(double bias_value) {
		this.bias_value = bias_value;
		this.outgoingConnections = new HashSet<>();
	}
	
	@Override
	public double getOutput() {
		return this.bias_value;
	}

	@Override
	public Map<Neuron, Double> incommingReferences() {
		return Collections.emptyMap();
	}

	@Override
	public Set<Neuron> outgoingReferences() {
		return this.outgoingConnections;
	}
	
	@Override
	public boolean addIncommingReference(Neuron n, double init_weight) {
		throw new UnsupportedOperationException("Bias nodes do not have incomming connections");
	}
	
	@Override
	public boolean addOutgoingReference(Neuron n) {
		return this.outgoingConnections.add(n);
	}

	@Override
	public void calculateLocalPotential() {
		// Do nothing here...
	}

	@Override
	public boolean adjustWeight(Neuron n, double new_weight) {
		return false;
	}
	
	@Override
	public int compareTo(Neuron n){ 
		return this.hashCode() - n.hashCode();
	}

}
