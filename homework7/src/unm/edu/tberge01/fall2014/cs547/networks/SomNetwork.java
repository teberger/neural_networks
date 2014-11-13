package unm.edu.tberge01.fall2014.cs547.networks;

import java.util.List;
import java.util.Random;
import java.util.Vector;

import unm.edu.tberge01.fall2014.cs547.neurons.InputNeuron;
import unm.edu.tberge01.fall2014.cs547.neurons.Neuron;
import unm.edu.tberge01.fall2014.cs547.neurons.SomNeuron;

public class SomNetwork extends Lattice2d {

	public int iter;
	public double sigma;
	public double eta;
	public final double tau1;
	public final double tau2;
	
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
		
		this.eta = 0.1;
		this.tau1 = 100 / Math.log(this.sigma);
		this.tau2 = 100;
	}
	
	public List<SomNeuron> getNeurons() {
		return this.neuron_list;
	}
	
	public double[] getNeuronWeights(SomNeuron n) {
		double pos1 = n.incommingReferences().get(inputs[0]);
		double pos2 = n.incommingReferences().get(inputs[1]);
		
		return new double[]{pos1, pos2};
	}

	@Override
	public double train(Vector<Double> inputVector, Vector<Double> desiredValues) {
		//we don't care about desired values in SOM architecture
		assert(desiredValues == null);
		this.setInputs(inputVector);

		double minDiff = Double.POSITIVE_INFINITY;
		SomNeuron closestNeuron = null;

		for (SomNeuron n : this.neuron_list) { 
			double diff = 0;
			diff += Math.abs(this.inputs[0].getOutput() - n.incommingReferences().get(this.inputs[0]));
			diff += Math.abs(this.inputs[1].getOutput() - n.incommingReferences().get(this.inputs[1]));
			
			//the neuron with the highest activation 
			//has the weight vector closest to the input
			if (diff < minDiff) {
				minDiff = diff;
				closestNeuron = n;
			}
		}
		
		double sumOfDistanceMoved = 0;
		
		for(SomNeuron n : this.neuron_list) {
			for (Neuron input : n.incommingReferences().keySet()) {
				//current value
				double weight_i = n.incommingReferences().get(input);
				//incremental adjustment value
				double eta_n = expDecay(this.iter, this.eta, this.tau2);
				double sigma_n = expDecay(this.iter, this.sigma, this.tau1);
				double h = SomNeuron.getLateralMultiplier(closestNeuron, n, sigma_n);
				double diff = input.getOutput() - weight_i;
				double adjustment = eta_n * h * diff;
				sumOfDistanceMoved += Math.abs(adjustment);
				n.addWeightAdjustment(input, adjustment);
			}
		}
		
		//adjust eta and sigma parameters
		return sumOfDistanceMoved;
	}

	/**
	 * Gives a location of the neuron that was activated in terms of 
	 * lattice coordinates in the SOM.
	 */
	@Override
	public Vector<Double> classify(Vector<Double> inputVector) {
		return null;
	}
	
	public double expDecay(int iteration, double decayValue, double tau) {
		return decayValue * Math.exp(- (iteration/tau));
	}

}
