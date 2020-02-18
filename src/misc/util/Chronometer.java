package misc.util;

import java.util.ArrayList;
import java.util.Hashtable;

import misc.Misc;

import java.util.Enumeration;

public final class Chronometer
{
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	private static Hashtable<String, Chronometer> chronometers = new Hashtable<String, Chronometer>();

	/****************************************************************/
	/************************ Main functions ************************/
	/****************************************************************/
	private static Chronometer getChronometer(String key)
	{
		if (Chronometer.chronometers.get(key) == null)
			Chronometer.chronometers.put(key, new Chronometer());
		return Chronometer.chronometers.get(key);
	}
	public static void start(String key)
	{
		Chronometer.getChronometer(key).start();
	}
	public static void finish(String key)
	{
		Chronometer.getChronometer(key).finish();
	}
	public static long getCounter(String key)
	{
		return Chronometer.getChronometer(key).getCounter();
	}
	public static long getTime(String key)
	{
		return Chronometer.getChronometer(key).getMilliseconds();
	}

	public static void showTimes()
	{
		System.out.println(Chronometer.getTimes());
	}
	public static String getTimes()
	{
		String text = "";
		final String lineSeparator = System.lineSeparator();

		text += "-------------------------------" + lineSeparator;
		text += "------------ Times ------------" + lineSeparator;
		text += "-------------------------------" + lineSeparator;

		final Enumeration<String> keys = Chronometer.chronometers.keys();
		final ArrayList<String> keys0 = new ArrayList<String>();
		final ArrayList<Chronometer> chronometers = new ArrayList<Chronometer>();

		for (int chronometerIndex = 0; chronometerIndex < Chronometer.chronometers.size(); chronometerIndex++)
		{
			final String key = keys.nextElement();
			final Chronometer chronometer = Chronometer.chronometers.get(key);

			int chronometerIndex2 = 0;
			for (; chronometerIndex2 < chronometers.size(); chronometerIndex2++)
				if (chronometers.get(chronometerIndex2).getMilliseconds() < chronometer.getMilliseconds())
					break;

			keys0.add(chronometerIndex2, key);
			chronometers.add(chronometerIndex2, chronometer);
		}

		for (int i = 0; i < chronometers.size(); i++)
		{
			final Chronometer chronometer = chronometers.get(i);
			final long counter = chronometer.getCounter();
			final long milliseconds = chronometer.getMilliseconds();

			text += keys0.get(i) + " was executed " + counter + " times in " + milliseconds + " milliseconds, which is " + Misc.round(1000.0 * milliseconds / counter, 2) + " microseconds each time" + lineSeparator;
		}

		return text;
	}
	public static void clearAll()
	{
		Chronometer.chronometers.clear();
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private long milliseconds = 0;
	private long millisecondStart = 0;
	private long counter = 0;
	private long recursiveCounter = 0;

	/****************************************************************/
	/************************** Constructor *************************/
	/****************************************************************/
	private Chronometer()
	{
		
	}

	/****************************************************************/
	/************************ Main functions ************************/
	/****************************************************************/
	private void start()
	{
		if (this.recursiveCounter == 0)
			this.millisecondStart = System.currentTimeMillis();
		this.recursiveCounter++;
		this.counter++;
	}
	private void finish()
	{
		if (this.recursiveCounter == 0)
			throw new RuntimeException("The chronometer is not running");
		this.recursiveCounter--;
		if (this.recursiveCounter == 0)
			this.milliseconds += System.currentTimeMillis() - this.millisecondStart;
	}
	private long getCounter()
	{
		return this.counter;
	}
	private long getMilliseconds()
	{
		return this.milliseconds;
	}
}