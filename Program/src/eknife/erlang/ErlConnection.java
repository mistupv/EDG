package eknife.erlang;

import java.io.File;
import java.io.IOException;
import java.net.InetAddress;

import eknife.config.Config;
import eknife.misc.Misc;

import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

public class ErlConnection
{
	private final String fileName = Config.erlangConnectionFileName;
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

	public ErlConnection()
	{
		this.connect();
	}

	private void connect()
	{
		int id = 0;

		while (true)
			try
			{
				final String localhostName = InetAddress.getLocalHost().getHostName();

				this.clientNodeName = this.defaultClientNodeName + id;
				this.serverNodeName = this.defaultServerNodeName + id;
				this.cookie = this.defaultCookie + id;
				this.openServer();
				Misc.wait(1000);
				this.clientNode = new OtpSelf(this.clientNodeName, this.cookie);
				this.serverNode = new OtpPeer(this.serverNodeName + "@" + localhostName);
				this.connection = this.clientNode.connect(this.serverNode);
				break;
			}
			catch (Exception e)
			{
				id++;
			}
	}
	public void disconnect()
	{
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
		final String command = "./" + this.fileName + " " + this.serverNodeName + " " + this.cookie;
		final File launcherFile = Config.getLauncherFile();

		this.serverProcess = runtime.exec(command, null, launcherFile);
	}
	private void closeServer() throws Exception
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