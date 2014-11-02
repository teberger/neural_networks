package unm.edu.tberge01.fall2014.cs547.neurons;
import java.util.Map;
import java.util.Set;

public interface Neuron extends Comparable<Neuron>{
	public void calculateLocalPotential();
	public double getOutput();
	public Map<Neuron, Double> incommingReferences();
	public Set<Neuron> outgoingReferences();
	public boolean addOutgoingReference(Neuron n);
	public boolean addIncommingReference(Neuron n, double init_weight);
	public boolean adjustWeight(Neuron n, double new_weight);
}
