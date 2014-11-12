package unm.edu.tberge01.fall2014.cs547.networks;

import java.util.Vector;

public interface NeuralNetwork {
	/**
	 * 
	 * @param inputVector
	 * @param desiredValues
	 * @return RMS Error for the input value
	 */
	public double train(Vector<Double> inputVector, Vector<Double> desiredValues);
	
	/**
	 * 
	 * @param inputVector
	 * @return output of the last neurons in order 
	 */
	public Vector<Double> classify(Vector<Double> inputVector);
	
	public void setInputs(Vector<Double> inputs);
}
