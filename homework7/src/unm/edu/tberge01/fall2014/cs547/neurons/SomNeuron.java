package unm.edu.tberge01.fall2014.cs547.neurons;

import java.util.Vector;

import unm.edu.tberge01.fall2014.cs547.utils.VectorUtilities;

public class SomNeuron extends McCullochPittsNeuron implements Neuron {

	public static double getEuclideanDistance(SomNeuron n, SomNeuron m) {
		return VectorUtilities.eclidianSquaredNorm(VectorUtilities.subtract(n.lattice_position, m.lattice_position));
	}
	
	public static double getLateralMultiplier(SomNeuron n, SomNeuron m, double sigma) {
		double dist = getEuclideanDistance(n, m); 
		double ret = - (dist/(2 * Math.pow(sigma, 2)));
		
		return Math.exp(ret);
	}
	
	private Vector<Double> lattice_position;
	
	public SomNeuron(Vector<Double> lattice_position) {
		super();
		
		this.lattice_position = lattice_position;
	}
}
