package unm.edu.tberge01.fall2014.cs547.generators;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Random;
import java.util.Vector;

import unm.edu.tberge01.fall2014.cs547.neurons.Neuron;
import unm.edu.tberge01.fall2014.cs547.neurons.RadialBasis;
import unm.edu.tberge01.fall2014.cs547.utils.VectorUtilities;

public class RadialNeuronGenerator {

	public static Neuron[] generate(int numToGenerate, PresentationSet data) {
		double sigma = 1 / Math.sqrt(2*numToGenerate);

		Random rand = new Random(numToGenerate);
		
		int[] indices = new int[numToGenerate];
		for(int i = 0; i < numToGenerate; i++)  
			indices[i] = rand.nextInt(data.getSize());
		Arrays.sort(indices);
		
		Iterator<Presentation> iter = data.iterator();
		int iteration = 0;
		
		int i= 0;
		@SuppressWarnings("unchecked")
		Vector<Double>[] centers = new Vector[numToGenerate];
		
		while(i < centers.length) {
			Presentation center_i = iter.next();
			while (i < indices.length && iteration == indices[i]) {	
				centers[i] = center_i.getValues();
				i++;
			}
			iteration++;
		}
		
		double maxDist = findMaxDistance(centers);
		sigma *= maxDist;
		
		Neuron[] neuronCenters = new Neuron[numToGenerate];
		
		for (int j = 0; j < numToGenerate; j++) 
			neuronCenters[j] = new RadialBasis(centers[j], sigma);
		
		return neuronCenters;
	}
	
	private static double findMaxDistance(Vector<Double>[] centers) {
		double max = 0;
		
		for (Vector<Double> v1 : centers)
			for(Vector<Double> v2 : centers ) 
				if (v1 != v2) {
					double distance = VectorUtilities.eclidianSquaredNorm(VectorUtilities.subtract(v1, v2));
					if (max < distance)
						max = distance;
				}
			
		return max;
	}
}
