package misc.util;

import java.io.InputStream;
import java.io.InputStreamReader;

public class Flusher {
	public enum Output {Standard, Error}

	private final Process process;
	private final boolean collectStandardOutput;
	private final boolean collectErrorOutput;
	private String standardOutput = "";
	private String errorOutput = "";

	public Flusher(Process process)
	{
		this(process, false, false);
	}
	public Flusher(Process process, boolean collectStandardOutput, boolean collectErrorOutput)
	{
		this.process = process;
		this.collectStandardOutput = collectStandardOutput;
		this.collectErrorOutput = collectErrorOutput;
	}

	public String start()
	{
		new Thread() {
			public void run()
			{
				if (!Flusher.this.collectStandardOutput)
					Flusher.this.flush(Flusher.this.process.getInputStream());
				else
					Flusher.this.flush(Output.Standard, Flusher.this.process.getInputStream());
			}
		}.start();
		new Thread() {
			public void run()
			{
				if (!Flusher.this.collectErrorOutput)
					Flusher.this.flush(Flusher.this.process.getErrorStream());
				else
					Flusher.this.flush(Output.Error, Flusher.this.process.getErrorStream());
			}
		}.start();

		return null;
	}

	private void flush(InputStream inputStream)
	{
		try
		{
			while (inputStream.read() != -1)
				;
			inputStream.close();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}
	private void flush(Output output, InputStream inputStream)
	{
		try (final InputStreamReader isr = new InputStreamReader(inputStream))
		{
			int character;

			while ((character = isr.read()) != -1)
				this.addOutput(output, (char) character);
		}
		catch (Exception e)
		{
			
		}
	}

	public synchronized void addOutput(Output output, char character)
	{
		switch (output)
		{
			case Standard:
				this.standardOutput += character;
				break;
			case Error:
				this.errorOutput += character;
				break;
			default:
				throw new RuntimeException("Output not contemplated: " + output);
		}
	}
	public synchronized String getOutput(Output output)
	{
		switch (output)
		{
			case Standard:
				if (!this.collectStandardOutput)
					throw new RuntimeException("The standard output is not being collected");
				return this.standardOutput;
			case Error:
				if (!this.collectErrorOutput)
					throw new RuntimeException("The error output is not being collected");
				return this.errorOutput;
			default:
				throw new RuntimeException("Output not contemplated: " + output);
		}
	}
}