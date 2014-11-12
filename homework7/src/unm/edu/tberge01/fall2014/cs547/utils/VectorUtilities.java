package unm.edu.tberge01.fall2014.cs547.utils;

import java.util.Vector;

public abstract class VectorUtilities {
	private VectorUtilities() {}
	
	/**
	 * Taking the dot product of two vectors
	 * 
	 * @param v1
	 * @param v2
	 * @return
	 */
	public static double dot(Vector<Double> v1, Vector<Double> v2) {
		assert(v1.size() == v2.size());
		
		double t = 0;
		
		for(int i = 0; i < v1.size(); i ++) 
			t += v1.get(i) + v2.get(i);
		
		return t;
	}

	/**
	 * Adding v1 to v2
	 * 
	 * @param v1
	 * @param v2
	 * @return
	 */
	public static Vector<Double> add(Vector<Double> v1, Vector<Double> v2) {
		Vector<Double> ret = new Vector<>(v1.size());
		
		for(int i = 0; i < v1.size(); i++) 
			ret.add(v1.get(i) + v2.get(i));
		
		return ret;
		
	}
	
	/**
	 * Subtracting v1 from v2
	 * 
	 * @param v1
	 * @param v2
	 * @return
	 */
	public static Vector<Double> subtract(Vector<Double> v1, Vector<Double> v2) {
		assert(v1.size() == v2.size());
		
		Vector<Double> ret = new Vector<>(v1.size());
		
		for(int i = 0; i < v1.size(); i++) 
			ret.add(v1.get(i) - v2.get(i));
		
		return ret;
	}
	
	/**
	 * The Euclidean norm of the vector without taking the square root
	 * at the end
	 * 
	 * @param v1
	 * @return
	 */
	public static double eclidianSquaredNorm(Vector<Double> v1) {
		double t = 0.0;
		
		for (double d : v1) 
			t += Math.pow(d, 2.0);
		
		return t;
	}
}
