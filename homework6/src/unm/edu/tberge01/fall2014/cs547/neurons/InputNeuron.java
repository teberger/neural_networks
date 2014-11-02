package unm.edu.tberge01.fall2014.cs547.neurons;

public class InputNeuron extends BiasNeuron {
	
	public InputNeuron() {
		super(0);
	}

	public void setInput(double value) {
		this.bias_value = value;
	}
}
