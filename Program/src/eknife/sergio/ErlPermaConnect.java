package eknife.sergio;

import java.io.File;
import java.io.IOException;
import java.net.InetAddress;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;
import eknife.misc.Misc;


public class ErlPermaConnect {
	
	private int id = 1;
    private OtpSelf client;
    private OtpPeer server;
    private OtpConnection connection;
    private Process serverProcess;
	
    public ErlPermaConnect()
    {
    	getReadyServer();
    }
    public void getReadyServer()
    {
    	while (true)
			try
			{
		    	openServer();
		    	Misc.wait(1000);
		    	init();
		    	break;
			}
		    catch (Exception e)
    		{
		    	this.id++;
    		}
    }
	private void openServer() throws Exception
	{
		final Runtime runtime = Runtime.getRuntime();
		final String command = "/Users/serperu/Desktop/Dropbox/UPV/ProgramSlicing(MagicToolSeparado)/e-knife/e-KnifePrimigenio/Launcher/erl.sh " + "servernode"+this.id+" "+"homer"+this.id; // Esta ruta varía entre máquinas
		// Aqui se obtiene la ruta de ejecucion
		final File launcherFile = new File("/Users/serperu/Desktop/Dropbox/UPV/ProgramSlicing(MagicToolSeparado)/e-knife/e-KnifePrimigenio/Tests/sergio/benchmarks/");
		this.serverProcess = runtime.exec(command, null, launcherFile);
		
	}
    public ErlPermaConnect(String ruta)
    {
    	getReadyServer(ruta);
    }
    public void getReadyServer(String ruta)
    {
    	while (true)
			try
			{
		    	openServer(ruta);
		    	Misc.wait(1000);
		    	init();
		    	break;
			}
		    catch (Exception e)
    		{
		    	this.id++;
    		}
    }
	private void openServer(String launcherFile) throws Exception
	{
		final Runtime runtime = Runtime.getRuntime();
		//final String command = "/Users/serperu/Desktop/Trabajo/Proyecto_Slicing/15_Programas/Generadores_de_entradas/erl.sh " + "servernode"+this.id+" "+"homer"+this.id;
		String s = new File("Cacafuti/tmp/erl.sh").getAbsolutePath();
		final String command = s + " servernode"+this.id+" "+"homer"+this.id;
		
		// Aqui se obtiene la ruta de ejecucion
		final File launchFile = new File(launcherFile);
		this.serverProcess = runtime.exec(command, null, launchFile);
		
	}
	public void closeServer()
    {
    	if (this.serverProcess != null)
    		this.serverProcess.destroy();
    }
	//Inicializar el server para que este preparado para conexiones del cliente
	private void init() throws IOException, OtpAuthException {
    	final String localhostName = InetAddress.getLocalHost().getHostName();
        this.client = new OtpSelf("client"+this.id,"homer"+this.id);
        this.server = new OtpPeer("servernode"+this.id+"@"+localhostName);
        this.connection = this.client.connect(this.server);
    }
	//Metodo que invoca el modulo, método y argumentos de Erlang
	public OtpErlangObject invokeErlang(String module, String func, OtpErlangObject... args)
	{
		try{
			this.connection.sendRPC(module, func, args);
			OtpErlangObject response = this.connection.receiveMsg().getMsg();
			return response;
		} catch (Exception e)
		{
			throw new RuntimeException(e);
		}
		
	}
	public OtpErlangObject invokeErlang(String module, String func, String... args)
	{
		return invokeErlang(module,func,getArgs(args));
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
