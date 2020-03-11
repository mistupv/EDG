package upv.slicing.eknife;

public final class Config
{
	private static final Config config = new Config();

	public static Config getConfig()
	{
		return Config.config;
	}

	private Config()
	{
	}

	public String getTestPathSuite() {
		return "./src/test/resources/suite/";
	}
}
