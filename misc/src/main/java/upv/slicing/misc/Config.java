package upv.slicing.misc;

import java.io.File;
import java.net.URLDecoder;

public abstract class Config
{
	/********************************************************************************************************************************/
	/************************************************************ OBJECT ************************************************************/
	/********************************************************************************************************************************/
	protected final String programPath;
	protected final String binariesPath;
	protected final String sourcesPath;
	protected final String rootPath;
	protected final String librariesPath;
	protected final String configurationPath;
	protected final String miscellaneaPath;
	protected final String temporaryPath;
	//ADDED
	protected final String testPath;
	protected final String testPathBenchmarks;
	protected final String testPathSuite;

	/****************************************************************/
	/************************* Constructor **************************/
	/****************************************************************/
	public Config()
	{
		final String projectPath = Misc.getProjectPath(this.getClass());
		this.programPath = this.calculateProgramPath(projectPath);
		this.binariesPath = this.programPath + "bin" + File.separator;
		this.sourcesPath = this.programPath + "src" + File.separator;

		//ADDED TESTPATH
		this.testPath = "./src/test/res/carlos/"; // MODIFY WHEN CHANGING THE SOURCE DIRECTORY
		this.testPathBenchmarks = "./src/test/res/carlos/"; // MODIFY WHEN CHANGING THE SOURCE DIRECTORY
		this.testPathSuite = "./src/test/res/"; // MODIFY WHEN CHANGING THE SOURCE DIRECTORY

		final boolean isJar = projectPath.endsWith(".jar!" + File.separator);
		this.rootPath = isJar ? this.programPath + "Resources" + File.separator : this.programPath;
		this.librariesPath = this.rootPath + "lib" + File.separator;
		this.configurationPath = this.rootPath + "cfg" + File.separator;
		this.miscellaneaPath = this.rootPath + "upv/slicing/misc" + File.separator;
		this.temporaryPath = this.rootPath + "tmp" + File.separator;
	}
	private String calculateProgramPath(String projectPath)
	{
		String programPath = projectPath;

		final String jarEnding = "!" + File.separator;
		final boolean isJar = projectPath.endsWith(".jar" + jarEnding);
		programPath = isJar ? programPath.substring(0, programPath.length() - jarEnding.length()) : programPath;
		programPath = isJar ? programPath.substring(0, programPath.lastIndexOf(File.separator)) : programPath;
		final boolean finishWithBin = programPath.endsWith(File.separator + "bin") || programPath.endsWith(File.separator + "bin" + File.separator);
		programPath = finishWithBin ? programPath.substring(0, programPath.lastIndexOf("bin")) : programPath;
		final boolean finishWithLib = programPath.endsWith(File.separator + "lib") || programPath.endsWith(File.separator + "lib" + File.separator);
		programPath = finishWithLib ? programPath.substring(0, programPath.lastIndexOf("lib")) : programPath;
		final boolean finishWithResources = programPath.endsWith(File.separator + "Resources") || programPath.endsWith(File.separator + "Resources" + File.separator);
		programPath = finishWithResources ? programPath.substring(0, programPath.lastIndexOf("Resources")) : programPath;

		programPath = programPath.endsWith(File.separator) ? programPath : programPath + File.separator;

		try
		{
			programPath = URLDecoder.decode(programPath, "UTF-8");
		}
		catch (Exception e)
		{
			
		}

		return programPath;
	}

	/****************************************************************/
	/**************************** Paths *****************************/
	/****************************************************************/
	public String getProgramPath()
	{
		return this.programPath;
	}
	public String getBinariesPath()
	{
		return this.binariesPath;
	}
	public String getSourcesPath()
	{
		return this.sourcesPath;
	}
	public String getLibrariesPath()
	{
		return this.librariesPath;
	}
	public String getConfigurationPath()
	{
		return this.configurationPath;
	}

	public String getMiscellaneaPath()
	{
		return this.miscellaneaPath;
	}

	public String getTemporaryPath()
	{
		return this.temporaryPath;
	}

	//ADDED getTestPath()
	public String getTestPath()
	{
		return this.testPath;
	}

	public String getTestPathBenchmarks()
	{
		return this.testPathBenchmarks;
	}

	public String getTestPathSuite()
	{
		return this.testPathSuite;
	}

	/****************************************************************/
	/**************************** Files *****************************/
	/****************************************************************/
	public File getProgramFile()
	{
		return new File(this.getProgramPath());
	}

	public File getBinariesFile()
	{
		return new File(this.getBinariesPath());
	}
	public File getSourcesFile()
	{
		return new File(this.getSourcesPath());
	}
	public File getLibrariesFile()
	{
		return new File(this.getLibrariesPath());
	}
	public File getConfigurationFile()
	{
		return new File(this.getConfigurationPath());
	}
	public File getMiscellaneaFile()
	{
		return new File(this.getMiscellaneaPath());
	}
	public File getTemporaryFile()
	{
		return new File(this.getTemporaryPath());
	}
}