package upv.slicing.edg;

public final class Config
{
	public static final int MAX_PRODUCTION_DEPTH = 5;
	public static final int MAX_STACK_SIZE = 9;
	public static final boolean PDG = false;
	public static final boolean APDG = false;
	public static final boolean PPDG = true;

	static {
		// Sanity checks for the configuration.
		assert PDG || APDG || PPDG;
		assert (PDG ? 1 : 0) + (APDG ? 1 : 0) + (PPDG ? 1 : 0) == 1;
	}

	private Config()
	{
		throw new IllegalStateException("No instances allowed!");
	}
}
