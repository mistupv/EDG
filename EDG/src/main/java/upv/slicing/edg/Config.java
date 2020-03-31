package upv.slicing.edg;

import upv.slicing.edg.graph.EDG;
import upv.slicing.edg.slicing.ConstrainedAlgorithm;
import upv.slicing.edg.slicing.ConstrainedPPDGAlgorithm;
import upv.slicing.edg.slicing.SlicingAlgorithm;

import java.util.function.Function;

public final class Config
{
	public static final int MAX_PRODUCTION_DEPTH = 5;
	public static final int MAX_STACK_SIZE = 9;
	public static final boolean PDG = false;
	public static final boolean APDG = false;
	public static final boolean PPDG = true;
	public static final Function<EDG, SlicingAlgorithm> CREATE_SLICING_ALGORITHM;

	static {
		// Sanity checks for the configuration.
		assert PDG || APDG || PPDG;
		assert (PDG ? 1 : 0) + (APDG ? 1 : 0) + (PPDG ? 1 : 0) == 1;
		if (PDG || APDG)
			CREATE_SLICING_ALGORITHM = ConstrainedAlgorithm::new;
		else if (PPDG)
			CREATE_SLICING_ALGORITHM = ConstrainedPPDGAlgorithm::new;
		else
			throw new IllegalStateException("Invalid PDG option selected!");
	}

	private Config()
	{
		throw new IllegalStateException("No instances allowed!");
	}
}
