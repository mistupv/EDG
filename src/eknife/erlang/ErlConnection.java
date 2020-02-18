package eknife.erlang;

import java.io.File;
import java.io.IOException;
import java.net.InetAddress;

import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

import eknife.config.Config;
import misc.util.Flusher;
import misc.util.Flusher.Output;

public class ErlConnection
{
	private final Config config = Config.getConfig();
	private final String defaultClientNodeName = "client";
	private final String defaultServerNodeName = "server";
	private final String defaultCookie = "erlang";

	private String clientNodeName;
	private String serverNodeName;
	private String cookie;
	private OtpSelf clientNode;
	private OtpPeer serverNode;
	private OtpConnection connection;
	private Process serverProcess;

	public boolean isConnected()
	{
		return this.connection != null && this.connection.isConnected();
	}

	public void connect()
	{
		if (this.isConnected())
			this.disconnect();

		int id = 0;

		while (true)
			try
			{
				final String localhostName = InetAddress.getLocalHost().getHostName();

				this.clientNodeName = this.defaultClientNodeName + id;
				this.serverNodeName = this.defaultServerNodeName + id;
				this.cookie = this.defaultCookie + id;
				this.openServer();
				this.clientNode = new OtpSelf(this.clientNodeName, this.cookie);
				this.serverNode = new OtpPeer(this.serverNodeName + "@" + localhostName);
				this.connection = this.clientNode.connect(this.serverNode);
				break;
			}
			catch (Exception e)
			{
				this.closeServer();
				id++;
			}
	}
	public void disconnect()
	{
		if (!this.isConnected())
			throw new RuntimeException("The connection is not established");

		try
		{
			if (this.connection != null)
				this.connection.close();
			this.closeServer();
		}
		catch (Exception e)
		{
			throw new RuntimeException(e);
		}
	}
	private void openServer() throws Exception
	{
		final Runtime runtime = Runtime.getRuntime();
		final String command = "/usr/local/bin/erl -sname " + this.serverNodeName + " -setcookie " + this.cookie;
		final File scriptsFile = this.config.getScriptsFile();
		this.serverProcess = runtime.exec(new String[] { "/bin/sh", "-c", command }, null, scriptsFile);

		final Flusher flusher = new Flusher(this.serverProcess, true, true);
		flusher.start();
		while (flusher.getOutput(Output.Standard).isEmpty() && flusher.getOutput(Output.Error).isEmpty())
			;
	}
	private void closeServer()
	{
		if (this.serverProcess != null)
			this.serverProcess.destroy();
	}

	public void send(String module, String function, OtpErlangObject[] message)
	{
		try
		{
			this.connection.sendRPC(module, function, message);
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}
	public OtpErlangObject receive()
	{
		try
		{
			return this.connection.receiveMsg().getMsg();
		}
		catch (Exception e)
		{
			throw new RuntimeException(e);
		}
	}
}