package unm.edu.tberge01.fall2014.cs547.neurons;

import java.util.HashSet;
import java.util.TreeMap;

public class McCullochPittsNeuron extends AbstractNeuron {
	
	public McCullochPittsNeuron() {
		super(new TreeMap<>(), new HashSet<>());
	}

	@Override
	public void calculateLocalPotential() {
		this.output = 0;
		
		for(Neuron n : this.inputConnections.keySet())
			this.output += this.inputConnections.get(n) * n.getOutput();
	}
}
