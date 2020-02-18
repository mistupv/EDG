package eknife.config;

import java.io.File;

import eknife.misc.Misc;

public final class Config
{
	/********************************************************************************************************************************/
	/************************************************************ STATIC ************************************************************/
	/********************************************************************************************************************************/
	private static String programPath;
	private static String binariesPath;
	private static String librariesPath;
	private static String sourcesPath;
	private static String temporalPath;
	private static String newBinariesPath;
	private static String replacedBinariesPath;

	private static String launcherPath;
	private static String testsPath;

	public static boolean constraintsActivated = true;
	public static String erlangConnectionFileName = "erl.sh";

	/****************************************************************/
	/********************* Initialize variables *********************/
	/****************************************************************/
	static
	{
		final String projectPath = Misc.getProjectPath();
		final int lastBinIndex = projectPath.lastIndexOf("bin");
		final boolean finishWithBin = projectPath.endsWith(File.separator + "bin") || projectPath.endsWith(File.separator + "bin" + File.separator);

		Config.programPath = lastBinIndex != -1 && finishWithBin ? projectPath.substring(0, lastBinIndex) : projectPath;
		Config.binariesPath = Config.programPath + "bin" + File.separator;
		Config.librariesPath = Config.programPath + "lib" + File.separator;
		Config.sourcesPath = Config.programPath + "src" + File.separator;
		Config.temporalPath = Config.programPath + "tmp" + File.separator;
		Config.newBinariesPath = Config.temporalPath + "nbin" + File.separator;
		Config.replacedBinariesPath = Config.temporalPath + "rbin" + File.separator;

		Config.launcherPath = Config.programPath + ".." + File.separator + "Launcher" + File.separator;
		Config.testsPath = Config.programPath + ".." + File.separator + "Tests" + File.separator + "simple" + File.pathSeparator;
	}

	/****************************************************************/
	/**************************** Paths *****************************/
	/****************************************************************/
	public static String getProgramPath()
	{
		return Config.programPath;
	}
	public static String getBinariesPath()
	{
		return Config.binariesPath;
	}
	public static String getLibrariesPath()
	{
		return Config.librariesPath;
	}
	public static String getSourcesPath()
	{
		return Config.sourcesPath;
	}
	public static String getTemporalPath()
	{
		return Config.temporalPath;
	}
	public static String getNewBinariesPath()
	{
		return Config.newBinariesPath;
	}
	public static String getReplacedBinariesPath()
	{
		return Config.replacedBinariesPath;
	}

	public static String getLauncherPath()
	{
		return Config.launcherPath;
	}
	public static String getTestsPath()
	{
		return Config.testsPath;
	}

	/****************************************************************/
	/**************************** Files *****************************/
	/****************************************************************/
	public static File getProgramFile()
	{
		return new File(Config.getProgramPath());
	}
	public static File getBinariesFile()
	{
		return new File(Config.getBinariesPath());
	}
	public static File getLibrariesFile()
	{
		return new File(Config.getLibrariesPath());
	}
	public static File getSourcesFile()
	{
		return new File(Config.getSourcesPath());
	}
	public static File getTemporalFile()
	{
		return new File(Config.getTemporalPath());
	}
	public static File getNewBinariesFile()
	{
		return new File(Config.getNewBinariesPath());
	}
	public static File getReplacedBinariesFile()
	{
		return new File(Config.getReplacedBinariesPath());
	}

	public static File getLauncherFile()
	{
		return new File(Config.getLauncherPath());
	}
	public static File getTestsFile()
	{
		return new File(Config.getTestsPath());
	}

	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	private Config()
	{
		
	}
}