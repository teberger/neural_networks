package unm.edu.tberge01.fall2014.cs547.runners;

import java.io.File;
import java.io.IOException;
import java.util.Vector;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

import unm.edu.tberge01.fall2014.cs547.generators.Presentation;
import unm.edu.tberge01.fall2014.cs547.generators.PresentationSet;
import unm.edu.tberge01.fall2014.cs547.generators.RandomSet;
import unm.edu.tberge01.fall2014.cs547.networks.SomNetwork;
import unm.edu.tberge01.fall2014.cs547.neurons.InputNeuron;
import unm.edu.tberge01.fall2014.cs547.neurons.SomNeuron;

public class SomRunner {
	
	public static final int input_size = 2;

	public static void main(String[] args) throws IOException {
		String trainingFile = args[0];
		
		PresentationSet training = new RandomSet(trainingFile, " ");

		int side_len = Integer.valueOf(args[1]);
		
		InputNeuron[] inputs = new InputNeuron[input_size];
		for(int i = 0; i < input_size; i++ ) 
			inputs[i] = new InputNeuron();
			
		
		SomNetwork network = new SomNetwork(side_len, side_len, inputs);
		
		int epoch = 0;
		int snapshot = 250;
		double averageMovement = 0;
		
		XYSeries weightChanges = new XYSeries("Average Weight Change");
		
		do {
			System.out.println("Epoch: " + epoch);
			averageMovement = 0.0;
			
			XYSeries presentation = new XYSeries("Data Presentations");
			
			for(Presentation p : training) {
				Vector<Double> inputValues= p.getValues();
				presentation.add(inputValues.get(0), inputValues.get(1));
				averageMovement += network.train(inputValues, null);
			}
			
			System.out.println("Total Movement: " + averageMovement);
			averageMovement /= training.getSize();
			weightChanges.add(epoch, averageMovement);
			
			if (epoch % snapshot == 0 || averageMovement <= 0.001) {
				XYSeries som = new XYSeries("SOM Data Points");
				for (SomNeuron n : network.getNeurons()) {
					double x = network.getNeuronWeights(n)[0];
					double y = network.getNeuronWeights(n)[1];
					som.add(x,y);
				}
				
				XYSeriesCollection col = new XYSeriesCollection();
				col.addSeries(som);
				col.addSeries(presentation);
				
				JFreeChart chart = ChartFactory.createScatterPlot(
						"SOM Overlay (" + epoch + " epoch, " + side_len + "x" + side_len + ")",
						"X1", "X2", col);
				
				File f = new File("./tau_div_10" + File.separatorChar);
				if (!f.exists())
					f.mkdirs();
				
				ChartUtilities.saveChartAsJPEG(new File(f.getAbsolutePath() + File.separatorChar + "epoch_" + epoch + ".jpg")
					, chart, 500, 500);
			}
			
			network.iter = ++epoch;
			
		} while (averageMovement > 0.001);
		
		JFreeChart chart = ChartFactory.createXYLineChart("Average Weight Change Over Epoch Iteration",
				"Epoch Number", "Average Weight Change", new XYSeriesCollection(weightChanges));
		
		ChartUtilities.saveChartAsJPEG(new File("./tau_div_10"+ File.separatorChar + "weightChange.jpg")
			, chart, 500, 300);
    }

}
