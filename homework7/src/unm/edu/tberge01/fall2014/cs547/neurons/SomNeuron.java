package unm.edu.tberge01.fall2014.cs547.neurons;

public class SomNeuron extends McCullochPittsNeuron {

	public static double getEuclideanDistance(SomNeuron n, SomNeuron m) {
		return Math.sqrt((n.x - m.x) * (n.x - m.x) + (n.y - m.y)*(n.y - m.y));
	}
	
	public static double getLateralMultiplier(SomNeuron n, SomNeuron m, double sigma) {
		double dist = getEuclideanDistance(n, m); 
		double ret = - (dist/(2 * Math.pow(sigma, 2)));
		
		return Math.exp(ret);
	}
	
	private final int x, y;
	
	public SomNeuron(int x, int y) {
		this.x = x;
		this.y = y;
	}
	
	public double[] getLatticePosition() {
		return new double[]{x,y};
	}
}
