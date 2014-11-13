package unm.edu.tberge01.fall2014.cs547.networks;

import java.util.Set;
import java.util.Vector;

import unm.edu.tberge01.fall2014.cs547.neurons.Neuron;
import unm.edu.tberge01.fall2014.cs547.neurons.RadialBasis;
import unm.edu.tberge01.fall2014.cs547.utils.VectorUtilities;

public class RadialBasisNetwork extends FullyConnectedNetwork {
	private final double eta;
	
	public RadialBasisNetwork(double eta, int inputSize, Set<Neuron>[] layers) {
		super(inputSize, layers);
		this.eta = eta;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public double train(Vector<Double> inputVector, Vector<Double> desiredValues) {
		this.setInputs(inputVector);
		
		for (Set<Neuron> layer : this.layers) {
			for(Neuron n : layer) 
				n.calculateLocalPotential();
		}
		
		//LMS adjustment of output neuron
		for(int i = 0; i < outputs.size(); i++) {
			Neuron n = outputs.get(i);
			for(Neuron input : n.incommingReferences().keySet()) {
				double error = (desiredValues.get(i) - n.getOutput());
				double delta_weight = eta *  input.getOutput() * error;
				
				n.addWeightAdjustment(input, delta_weight);
			}
		}
	
		//Center adjustment of RBF nodes
		Neuron closest = null;
		Vector<Double> closestVector = null;
		double dist = 1000000.0;
		
		for (Neuron n : this.layers[0]) {
			Vector<Double> center = ((RadialBasis) n).getCenter();
			
			if (VectorUtilities.eclidianSquaredNorm(VectorUtilities.subtract(inputVector, center)) < dist) {
				closest = n;
				closestVector = center;
			}
		}
		
		Vector<Double> adjustment = new Vector<Double>(closestVector.size());
		for(int i = 0; i < closestVector.size(); i++) {
			double val = this.eta * (inputVector.get(i) - closestVector.get(i)); 
			adjustment.add(val);
		}
		
		
		((RadialBasis) closest).adjustCenter(adjustment);

		return calculateError(desiredValues);
	}
	
	private double calculateError(Vector<Double> desireds) {
		assert(desireds.size() == this.outputs.size());
		
		double error = 0;
		for(int i = 0; i < desireds.size(); i++)
			error += 0.5 * Math.pow(desireds.get(i) - outputs.get(i).getOutput(), 2.0); 
	
		return error;
	}

}
