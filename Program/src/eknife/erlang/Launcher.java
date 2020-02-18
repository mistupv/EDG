package eknife.erlang;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;

public class Launcher
{
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
		final ErlConnection connection = new ErlConnection();
		connection.send(module, function, args);
		final OtpErlangObject response = connection.receive();
		connection.disconnect();

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