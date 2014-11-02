package unm.edu.tberge01.fall2014.cs547.generators;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

public class PresentationSet implements Iterable<Presentation> {
	
	private final List<String> lines;
	private final String delim;
	private final int size;
	
	public PresentationSet(String filepath, String delim) throws IOException {
		this.delim = delim;
		File f = new File(filepath);
		BufferedReader reader = new BufferedReader(new FileReader(f));
		
		lines = new ArrayList<>();
		
		String line = "";
		while (null != (line = reader.readLine())) {
			lines.add(line);
		}
		
		this.size = lines.size();
		
		reader.close();
	}
	
	public int getSize() {
		return this.size;
	}

	@Override
	public Iterator<Presentation> iterator() {
		int[] presentationOrder = new int[lines.size()];
		for(int i =0; i < presentationOrder.length; i++) 
			presentationOrder[i] = i;
		
		Collections.shuffle(Arrays.asList(presentationOrder, presentationOrder.length)); 
		
		return new Iterator<Presentation>() {
			private int iterCount = 0;
			private int[] ordering = presentationOrder;

			@Override
			public boolean hasNext() {
				return iterCount < lines.size();
			}

			@Override
			public Presentation next() {
				String line = lines.get(ordering[iterCount]);
				line = line.replaceAll(delim + "+", delim);
				String[] items = line.split(delim);
				
				int clazz =(int) Double.parseDouble(items[0]);
				
				Vector<Double> v = new Vector<Double>(items.length - 2);
				for (int i = 2; i < items.length; i++)
					v.add(Double.parseDouble(items[i]));
				
				iterCount++;
				return new Presentation(clazz, v);
			}

		};
	}
}
