package unm.edu.tberge01.fall2014.cs547.runners;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class GridGenerator {
	public static void main(String[] args) throws IOException {
		File f = new File("./Grid.txt");
		BufferedWriter writer = new BufferedWriter(new FileWriter(f));
		
		double xmin = -0.1, xmax = 1.7;
		
		for(double i = xmin; i <= xmax; i+= 0.01)
			for(double j = xmin; j <= xmax; j += 0.01)
				writer.write("1 pointless " + i + " " + j + "\n");
			
		
		writer.flush();
		writer.close();
		
	}
}
