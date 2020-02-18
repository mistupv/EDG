package misc.math;

import java.util.List;

public final class Statistics
{
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	/****************************************************************/
	/************************** Summation ***************************/
	/****************************************************************/
	public static double summation(List<Double> values)
	{
		final double[] array = Statistics.getArray(values);

		return Statistics.summation(array);
	}
	public static double summation(double[] values)
	{
		double summation = 0.0;

		for (double value : values)
			summation += value;

		return summation;
	}

	/****************************************************************/
	/*************************** Average ****************************/
	/****************************************************************/
	public static double average(List<Double> values)
	{
		final double[] array = Statistics.getArray(values);

		return Statistics.average(array);
	}
	public static double average(double[] values)
	{
		final double summation = Statistics.summation(values);

		return summation / (1.0 * values.length);
	}

	/****************************************************************/
	/********************** Standard deviation **********************/
	/****************************************************************/
	public static double standardDeviation(List<Double> values)
	{
		final double[] array = Statistics.getArray(values);

		return Statistics.standardDeviation(array);
	}
	public static double standardDeviation(double[] values)
	{
		double summatory = 0.0;
		final double average = Statistics.average(values);

		for (double value : values)
			summatory += Math.pow(value - average, 2);

		final double division = summatory / (1.0 * values.length);

		return Math.sqrt(division);
	}

	/****************************************************************/
	/****************** Sample standard deviation *******************/
	/****************************************************************/
	public static double sampleStandardDeviation(List<Double> values)
	{
		final double[] array = Statistics.getArray(values);

		return Statistics.sampleStandardDeviation(array);
	}
	public static double sampleStandardDeviation(double[] values)
	{
		double summatory = 0.0;
		final double average = Statistics.average(values);

		for (double value : values)
			summatory += Math.pow(value - average, 2);

		final double division = summatory / (values.length - 1.0);

		return Math.sqrt(division);
	}

	/****************************************************************/
	/******************* Coefficient of variation *******************/
	/****************************************************************/
	public static double coefficientOfVariation(List<Double> values)
	{
		final double[] array = Statistics.getArray(values);

		return Statistics.coefficientOfVariation(array);
	}
	public static double coefficientOfVariation(double[] values)
	{
		final double standardDeviation = Statistics.standardDeviation(values);
		final double average = Statistics.average(values);

		return standardDeviation / average;
	}

	/************************************************************************************************/
	/****************************************** Intervals *******************************************/
	/************************************************************************************************/
	/****************************************************************/
	/********************** Confidence interval *********************/
	/****************************************************************/
	public static double[] confidenceInterval(List<Double> values, int confidenceLevel)
	{
		final double[] array = Statistics.getArray(values);

		return Statistics.confidenceInterval(array, confidenceLevel);
	}
	public static double[] confidenceInterval(double[] values, int confidenceLevel)
	{
		final double average = Statistics.average(values);
		final double sampleStandardDeviation = Statistics.sampleStandardDeviation(values);
		final int samples = values.length;

		return Statistics.confidenceInterval(average, sampleStandardDeviation, samples, confidenceLevel);
	}
	public static double[] confidenceInterval(double average, double standardDeviation, int confidenceLevel)
	{
		return Statistics.confidenceInterval(average, standardDeviation, 1, confidenceLevel);
	}
	public static double[] confidenceInterval(double average, double sampleStandardDeviation, int samples, int confidenceLevel)
	{
		final double[] confidenceInterval = new double[2];
		final double z = Statistics.getGaussianZ(0, 1, confidenceLevel);
		final double deviation = z * (sampleStandardDeviation / Math.sqrt(samples));

		confidenceInterval[0] = average - deviation;
		confidenceInterval[1] = average + deviation;

		return confidenceInterval;
	}

	/****************************************************************/
	/********************** Prediction interval *********************/
	/****************************************************************/
	public static double[] predictionInterval(List<Double> values, int predictionLevel)
	{
		final double[] array = Statistics.getArray(values);

		return Statistics.predictionInterval(array, predictionLevel);
	}
	public static double[] predictionInterval(double[] values, int predictionLevel)
	{
		final double average = Statistics.average(values);
		final double sampleStandardDeviation = Statistics.sampleStandardDeviation(values);

		return Statistics.predictionInterval(average, sampleStandardDeviation, predictionLevel);
	}
	public static double[] predictionInterval(double average, double sampleStandardDeviation, int predictionLevel)
	{
		return Statistics.confidenceInterval(average, sampleStandardDeviation, 1, predictionLevel);
	}

	/************************************************/
	/****************** Gaussian Z ******************/
	/************************************************/
	public static double getGaussianZ(double average, double variance, double level)
	{
		final double significanceLevel = (100.0 - level) / 100.0;
		final double halfSignificanceLevel = significanceLevel / 2.0;
		final double value = 1.0 - halfSignificanceLevel;

		// http://www.vadenumeros.es/sociales/tabla-distribucion-normal-tipificada.htm
		if (value == 0.925)	// 85 %
			return 1.44;
		if (value == 0.9299999999999999)	// 86 %
			return 1.48;
		if (value == 0.935)	// 87 %
			return 1.51;
		if (value == 0.94)	// 88 %
			return 1.55;
		if (value == 0.945)	// 89 %
			return 1.6;
		if (value == 0.95)	// 90 %
			return 1.65;
		if (value == 0.955)	// 91 %
			return 1.7;
		if (value == 0.96)	// 92 %
			return 1.75;
		if (value == 0.965)	// 93 %
			return 1.81;
		if (value == 0.97)	// 94 %
			return 1.88;
		if (value == 0.975)	// 95 %
			return 1.96;
		if (value == 0.98)	// 96 %
			return 2.05;
		if (value == 0.985)	// 97 %
			return 2.17;
		if (value == 0.99)	// 98 %
			return 2.33;
		if (value == 0.995)	// 99 %
			return 2.58;
		return 1.5;
	}

	/************************************************************************************************/
	/****************************************** Auxiliary *******************************************/
	/************************************************************************************************/
	public static double[] getArray(List<Double> values)
	{
		final double[] array = new double[values.size()];
		int listIndex = 0;

		for (double value : values)
			array[listIndex++] = value;

		return array;
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private Statistics()
	{
		
	}
}