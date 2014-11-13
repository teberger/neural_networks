package unm.edu.tberge01.fall2014.cs547.generators;

import java.io.IOException;
import java.util.Iterator;
import java.util.Random;
import java.util.Vector;

public class RandomSet extends PresentationSet {

	private int numberOfCalls = 0;
	public RandomSet(String filepath, String delim) throws IOException {
		super(filepath, delim);
	}

	@Override
	public Iterator<Presentation> iterator() {
		return new Iterator<Presentation>() {
			private int count = 0;
			private Random rand =  new Random(numberOfCalls+=31*numberOfCalls);
			
			@Override
			public boolean hasNext() {
				return count < size;
			}
			
			@Override
			public Presentation next() {
				if (!hasNext())
					return null;
				
				int idx = rand.nextInt(size);
				String line = lines.get(idx);
				line = line.replaceAll(delim + "+", delim);
				String[] items = line.split(delim);
				
				Vector<Double> v = new Vector<Double>(2);
				v.add(Double.parseDouble(items[2]));
				v.add(Double.parseDouble(items[3]));
				
				count++;
				return new Presentation(0, v);
			}
		};
	}
}
