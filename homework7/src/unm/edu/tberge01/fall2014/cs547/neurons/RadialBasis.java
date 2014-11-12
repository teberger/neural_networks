package unm.edu.tberge01.fall2014.cs547.neurons;
import java.util.HashSet;
import java.util.TreeMap;
import java.util.Vector;

import unm.edu.tberge01.fall2014.cs547.utils.VectorUtilities;

public class RadialBasis extends AbstractNeuron {
	
	private Vector<Double> center;
	private final double spread;
	
	public RadialBasis(Vector<Double> center, double spread) {
		super(new TreeMap<>(), new HashSet<>());
		
		this.center = center;
		this.spread = spread;
	}

	@Override
	public void calculateLocalPotential() {
		
		int size = this.inputConnections.size();
		Vector<Double> inputVector = new Vector<>(size);
		
		for(Neuron n : this.inputConnections.keySet()) {
			inputVector.add(n.getOutput());
		}
		
		Vector<Double> xbar = VectorUtilities.subtract(inputVector, center);
		double norm = VectorUtilities.eclidianSquaredNorm(xbar);
		double power = - norm / (2 * Math.pow(spread, 2));
		
		this.output = Math.exp(power);
	}
	
	public Vector<Double> getCenter() {
		return this.center;
	}
	

	public void adjustCenter(Vector<Double> adjustment) {
		this.center = VectorUtilities.add(adjustment, center);
	}
}
