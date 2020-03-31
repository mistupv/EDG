package upv.slicing.edg;

import upv.slicing.edg.edge.ControlEdgeGenerator;
import upv.slicing.edg.edge.EdgeGenerator;
import upv.slicing.edg.graph.EDG;

import java.util.function.Function;

public final class Config
{
	public static final int MAX_PRODUCTION_DEPTH = 5;
	public static final int MAX_STACK_SIZE = 9;
	public static final Function<EDG, EdgeGenerator> CONTROL_EDGE_GENERATOR_FUNCTION = ControlEdgeGenerator.Classic::new;

	private Config()
	{
		throw new IllegalStateException("No instances allowed!");
	}
}
