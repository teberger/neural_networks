package unm.edu.tberge01.fall2014.cs547.generators;

import java.util.Vector;

public class Presentation {
	private final int clazz;
	private final Vector<Double> values;
	
	public Presentation(int clazz, Vector<Double> values) {
		this.clazz = clazz;
		this.values = values;
	}
	
	public int getClazz() {
		return this.clazz;
	}
	
	public Vector<Double> getValues() {
		return this.values;
	}
}
