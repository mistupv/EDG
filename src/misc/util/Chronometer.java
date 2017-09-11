package misc.util;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Set;

import misc.Misc;

public final class Chronometer
{
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	private static final Map<String, Chronometer> chronometers = new Hashtable<String, Chronometer>();

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
	public static void stop(String key)
	{
		Chronometer.getChronometer(key).stop();
	}
	public static long getCount(String key)
	{
		return Chronometer.getChronometer(key).getCount();
	}
	public static long getTime(String key)
	{
		return Chronometer.getChronometer(key).getMilliseconds();
	}

	public static void show()
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

		final Set<String> chronometerKeys = Chronometer.chronometers.keySet();
		final List<String> keys = new ArrayList<String>();
		final List<Chronometer> chronometers = new ArrayList<Chronometer>();

		for (String chronometerKey : chronometerKeys)
		{
			final Chronometer chronometer = Chronometer.chronometers.get(chronometerKey);
			final long chronometerMilliseconds = chronometer.getMilliseconds();

			int chronometerIndex = 0;
			for (; chronometerIndex < chronometers.size(); chronometerIndex++)
				if (chronometers.get(chronometerIndex).getMilliseconds() < chronometerMilliseconds)
					break;

			keys.add(chronometerIndex, chronometerKey);
			chronometers.add(chronometerIndex, chronometer);
		}

		for (int chronometerIndex = 0; chronometerIndex < chronometers.size(); chronometerIndex++)
		{
			final String key = keys.get(chronometerIndex);
			final Chronometer chronometer = chronometers.get(chronometerIndex);
			final long count = chronometer.getCount();
			final long milliseconds = chronometer.getMilliseconds();
			final double microseconds = Misc.round(1000.0 * milliseconds / count, 2);
			final String timesStr = count == 1 ? "time" : "times";
			final String millisecondsStr = milliseconds == 1 ? "millisecond" : "milliseconds";
			final String microsecondsStr = microseconds == 1.0 ? "microsecond" : "microseconds";

			text += key + " was executed " + count + " " + timesStr + " in " + milliseconds + " " + millisecondsStr + ", which is " + microseconds + " " + microsecondsStr + " each time" + lineSeparator;
		}

		return text;
	}
	public static void clear()
	{
		Chronometer.chronometers.clear();
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private long milliseconds = 0;
	private long millisecondStart = 0;
	private long count = 0;
	private long recursiveCount = 0;

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
		if (this.recursiveCount == 0)
			this.millisecondStart = System.currentTimeMillis();
		this.recursiveCount++;
		this.count++;
	}
	private void stop()
	{
		if (this.recursiveCount == 0)
			throw new RuntimeException("The chronometer is not running");
		this.recursiveCount--;
		if (this.recursiveCount == 0)
			this.milliseconds += System.currentTimeMillis() - this.millisecondStart;
	}
	private long getCount()
	{
		return this.count;
	}
	private long getMilliseconds()
	{
		return this.milliseconds;
	}
}