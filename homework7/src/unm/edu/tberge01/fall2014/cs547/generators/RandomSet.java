package unm.edu.tberge01.fall2014.cs547.generators;

import java.io.IOException;
import java.util.Iterator;
import java.util.Random;
import java.util.Vector;

public class RandomSet extends PresentationSet {

	public RandomSet(String filepath, String delim) throws IOException {
		super(filepath, delim);
	}

	@Override
	public Iterator<Presentation> iterator() {
		return new Iterator<Presentation>() {
			private int count = 0;
			private Random rand =  new Random();
			
			@Override
			public boolean hasNext() {
				return count < size;
			}
			
			@Override
			public Presentation next() {
				String line = lines.get(rand.nextInt(size));
				line = line.replaceAll(delim + "+", delim);
				String[] items = line.split(delim);
				
				Vector<Double> v = new Vector<Double>(2);
				v.add(Double.parseDouble(items[2]));
				v.add(Double.parseDouble(items[3]));
				
				return new Presentation(0, v);
			}
		};
	}
}
