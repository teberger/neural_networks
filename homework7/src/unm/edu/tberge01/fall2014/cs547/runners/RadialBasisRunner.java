package unm.edu.tberge01.fall2014.cs547.runners;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.data.time.RegularTimePeriod;
import org.jfree.data.time.TimeSeries;
import org.jfree.data.time.TimeSeriesCollection;
import org.jfree.data.time.TimeSeriesDataItem;
import org.jfree.data.xy.XYDataItem;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

import unm.edu.tberge01.fall2014.cs547.generators.Presentation;
import unm.edu.tberge01.fall2014.cs547.generators.PresentationSet;
import unm.edu.tberge01.fall2014.cs547.generators.RadialNeuronGenerator;
import unm.edu.tberge01.fall2014.cs547.networks.RadialBasisNetwork;
import unm.edu.tberge01.fall2014.cs547.neurons.McCullochPittsNeuron;
import unm.edu.tberge01.fall2014.cs547.neurons.Neuron;

public class RadialBasisRunner {

	public static void main(String[] args) throws IOException {
		
		double eta = Double.parseDouble(args[0]);
		String trainingFile = args[1];
		String testingFile = args[2];
		
		PresentationSet training = new PresentationSet(trainingFile, " ");
		PresentationSet testing = new PresentationSet(testingFile, " ");
		
		int numberOfHiddenNodes = Integer.parseInt(args[3]);
		Neuron[] radials = RadialNeuronGenerator.generate(numberOfHiddenNodes, training);
		Set<Neuron>[] layers = new Set[2];
		layers[0] = new HashSet<Neuron>();
		layers[0].addAll(Arrays.asList(radials));
		
		Neuron output = new McCullochPittsNeuron();
		layers[1] = new HashSet<Neuron>();
		layers[1].add(output);
		
		int inputSize = 2;
		RadialBasisNetwork network = new RadialBasisNetwork(eta, inputSize, layers);
		
		XYSeries trainingRms = new XYSeries("Training RMS Error");
		XYSeries testingRms = new XYSeries("Testing RMS Error");

		double epochRMS = 10000.0;
		double firstEpochRMS = 1.0;
		int epochNumber = 0;
		
		while (/*epochRMS > 0.1 * firstEpochRMS && */epochNumber < 2000) {
			System.out.println("Epoch# " + epochNumber);
			epochNumber++;
			epochRMS = 0;
			Iterator<Presentation> iter = training.iterator();
			for(Presentation x; iter.hasNext(); ){
				x = iter.next();
				
				Vector<Double> desired = new Vector<>(1);
				desired.add((double)x.getClazz());
				
				epochRMS += network.train(x.getValues(), desired);
			}
			
			epochRMS = Math.sqrt(epochRMS);
			
			if(epochNumber == 1)
				firstEpochRMS = epochRMS;
			
			trainingRms.add(epochNumber, epochRMS);	

//			double testingRMS = 0;
//			iter = testing.iterator();
//			for(Presentation x; iter.hasNext(); ){
//				x = iter.next();
//				
//				Vector<Double> clazz = network.classify(x.getValues());
//				//System.out.println("Class Presented: " + x.getClazz() + ", Class Determined: " + clazz.get(0));
//				
//				testingRMS += Math.pow((double)x.getClazz() - clazz.get(0), 2);
//			}
//			testingRms.add(epochNumber, Math.sqrt(testingRMS));
		}
		
//		XYSeriesCollection coll = new XYSeriesCollection();
//		coll.addSeries(testingRms);
//		coll.addSeries(trainingRms);
//		
//		JFreeChart chart = ChartFactory.createXYLineChart("RMS Error ("+ numberOfHiddenNodes + " Neurons)", 
//				"Epoch Number", 
//				"RMS Error", 
//				coll);
//		
//		ChartUtilities.saveChartAsJPEG(new File("./RMS_" + numberOfHiddenNodes + "_nodes.jpg"), chart, 500, 300);
//		

		File gridOutput = new File("./GridOutput_" + numberOfHiddenNodes + "_nodes.txt");
		BufferedWriter writer = new BufferedWriter(new FileWriter(gridOutput));

		Iterator<Presentation> iter = testing.iterator();
		for (Presentation x; iter.hasNext(); ) {
			x = iter.next();
			
			Vector<Double> classification = network.classify(x.getValues());
			double c = classification.get(0);
			double x1 = x.getValues().get(0);
			double x2 = x.getValues().get(1);
			
			writer.write(String.format("%f %f %f\n", c, x1, x2));
		}
		
		writer.flush();
		writer.close();
			
	}
}
