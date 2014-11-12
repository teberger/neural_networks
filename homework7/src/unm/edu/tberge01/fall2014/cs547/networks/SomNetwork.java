package unm.edu.tberge01.fall2014.cs547.networks;

import java.util.Random;
import java.util.Vector;

import unm.edu.tberge01.fall2014.cs547.neurons.InputNeuron;
import unm.edu.tberge01.fall2014.cs547.neurons.Neuron;
import unm.edu.tberge01.fall2014.cs547.neurons.SomNeuron;
import unm.edu.tberge01.fall2014.cs547.utils.VectorUtilities;

public class SomNetwork extends Lattice2d {

	private int iter;
	private double sigma;
	private double eta;
	private final double tau1;
	private final double tau2;
	
	public SomNetwork(int n, int m, InputNeuron[] inputs) {
		super(n, m, inputs, new Random(n*m));
		
		this.iter = 0;
		
		//First set sigma to encompass all neurons
		this.sigma = 0;
		
		for(SomNeuron n1 : this.neuron_list) {
			for (SomNeuron n2 : this.neuron_list) {
				double dist = SomNeuron.getEuclideanDistance(n1,n2);
				if (dist > sigma) 
					sigma = dist;
			}
		}
		
		this.eta = this.sigma;
		this.tau1 = 1000 / Math.log(this.sigma);
		this.tau2 = 1000;
	}

	@Override
	public double train(Vector<Double> inputVector, Vector<Double> desiredValues) {
		return 0;
	}

	@Override
	public Vector<Double> classify(Vector<Double> inputVector) {
		return null;
	}
	
	private double expDecay(int iteration, double decayValue, double tau) {
		return decayValue * Math.exp(- (iteration/tau));
	}

}
