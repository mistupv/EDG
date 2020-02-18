package test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class Test
{
	public static Test test;

	private static Map<String, Test> tests = new HashMap<String, Test>();
	public static Test getTest(String testName)
	{
		Test test = Test.tests.get(testName);

		if (test == null)
		{
			test = new Test(testName);
			Test.tests.put(testName, test);
		}

		return test;
	}
	public static List<Test> getTests()
	{
		final List<Test> tests = new LinkedList<Test>();
		final Object[] keys = Test.tests.keySet().toArray();

		Arrays.sort(keys);
		for (Object key : keys)
		{
			final Test test = Test.tests.get(key);
			tests.add(test);
		}

		return tests;
	}
	public static void clearTests()
	{
		Test.tests.clear();
	}

	private String name;
	private int loops;
	private int loopsExecuted;
	private int iterationsExecuted;
	private long executionTime;
	private long loopTimer;
	private long bodyTimer;

	private LinkedList<Long> currentLoopNano = new LinkedList<Long>();
	private LinkedList<Long> currentLoopTimer = new LinkedList<Long>();
	private LinkedList<Long> currentLoopTimes = new LinkedList<Long>();
	private LinkedList<Long> currentBodyNano = new LinkedList<Long>();
	private LinkedList<Long> currentBodyTimer = new LinkedList<Long>();
	private LinkedList<Long> currentBodyTimes = new LinkedList<Long>();

	public Test(String name)
	{
		this.name = name;
	}

	public String getName()
	{
		return this.name;
	}
	public int getLoops()
	{
		return this.loops;
	}
	public int getLoopsExecuted()
	{
		return this.loopsExecuted;
	}
	public int getIterationsExecuted()
	{
		return this.iterationsExecuted;
	}
	public long getExecutionTime()
	{
		return this.executionTime;
	}
	public long getLoopTimer()
	{
		return this.loopTimer;
	}
	public long getBodyTimer()
	{
		return this.bodyTimer;
	}

	public void setLoops(int numLoops)
	{
		this.loops = numLoops;
	}
	public void setExecutionTime(long nanosegs)
	{
		this.executionTime = nanosegs;
	}

	public void startLoop()
	{
		this.currentLoopTimer.add(0l);
		this.currentLoopTimes.add(0l);
		this.currentBodyTimer.add(0l);
		this.currentBodyTimes.add(0l);
		this.loopsExecuted++;

		this.startTimer();
	}
	public void startIteration()
	{
		this.stopTimer();
		this.startBody();
		this.iterationsExecuted++;
	}
	public void endIteration()
	{
		this.stopBody();
		this.startTimer();
	}
	public void endLoop()
	{
		this.stopTimer();

		long currentLoopTimer = this.currentLoopTimer.removeLast();
		long currentLoopTimes = this.currentLoopTimes.removeLast();
		this.loopTimer += currentLoopTimer / currentLoopTimes;

		long currentBodyTimer = this.currentBodyTimer.removeLast();
		long currentBodyTimes = this.currentBodyTimes.removeLast();
		if (currentBodyTimes > 0)
			this.bodyTimer += currentBodyTimer / currentBodyTimes;
	}

	private void startTimer()
	{
		long currentTimes = this.currentLoopTimes.removeLast();
		this.currentLoopTimes.add(currentTimes + 1);

		long currentNano = System.nanoTime();
		this.currentLoopNano.add(currentNano);
	}
	private void stopTimer()
	{
		long currentNano = System.nanoTime() - this.currentLoopNano.removeLast();
		long currentTimer = this.currentLoopTimer.removeLast();

		this.currentLoopTimer.add(currentTimer + currentNano);
	}
	private void startBody()
	{
		long currentTimes = this.currentBodyTimes.removeLast();
		this.currentBodyTimes.add(currentTimes + 1);

		long currentNano = System.nanoTime();
		this.currentBodyNano.add(currentNano);
	}
	private void stopBody()
	{
		long currentNano = System.nanoTime() - this.currentBodyNano.removeLast();
		long currentTimer = this.currentBodyTimer.removeLast();

		this.currentBodyTimer.add(currentTimer + currentNano);
	}

	public String getInfo()
	{
		String info = this.name + " => ";

		info += "\t" + this.loops + " loops";
		info += "\t" + this.loopsExecuted + " loops executed";
		info += "\t" + this.iterationsExecuted + " iterations";
		info += "\t" + this.executionTime + " nanosegs";
		info += "\t" + this.loopTimer + " nanosegs";
		info += "\t" + this.bodyTimer + " nanosegs";

		return info;
	}
}