package eknife.erlang.launcher;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;

public class Launcher
{
	private static final Launcher launcher = new Launcher();
	public static Launcher getLauncher()
	{
		return Launcher.launcher;
	}

	private final ErlConnection erlConnection = new ErlConnection();

	private Launcher()
	{
		
	}

	public void open()
	{
		this.erlConnection.connect();
	}
	public void close()
	{
		this.erlConnection.disconnect();
	}

	public OtpErlangObject launch(String module, String function)
	{
		final OtpErlangObject[] arguments = new OtpErlangObject[0];

		return this.launch(module, function, arguments);
	}
	public OtpErlangObject launch(String module, String function, String... args)
	{
		final OtpErlangObject[] arguments = this.getArgs(args);

		return this.launch(module, function, arguments);
	}
	public OtpErlangObject launch(String module, String function, OtpErlangObject... args)
	{
		final boolean connected = this.erlConnection.isConnected();

		if (!connected)
			this.erlConnection.connect();
		this.erlConnection.send(module, function, args);
		final OtpErlangObject response = this.erlConnection.receive();
		if (!connected)
			this.erlConnection.disconnect();

		return response;
	}
	private OtpErlangObject[] getArgs(String... args)
	{
		final int argsLength = args.length;
		final OtpErlangObject[] arguments = new OtpErlangObject[argsLength];

		for (int argIndex = 0; argIndex < argsLength; argIndex++)
			arguments[argIndex] = new OtpErlangAtom(args[argIndex]);

		return arguments;
	}
}