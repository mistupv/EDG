package upv.slicing.edg.constraint;

import upv.slicing.edg.slicing.Phase;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

public abstract class Constraint
{
	public abstract boolean equals(Object object);
	public abstract String toString();

	protected List<Constraints> wrap(Constraints... constraints)
	{
		return new LinkedList<>(Arrays.asList(constraints));
	}

	protected void check(Phase phase1, Phase phase2)
	{
		if (!phase1.isInstanceof(phase2))
			throw new RuntimeException("Constraint situation not contemplated");
	}
	protected <R> void check(R obj1, R obj2)
	{
		if (obj1 == null && obj2 == null)
			return;
		if (obj1 == null || obj2 == null)
			throw new RuntimeException("Constraint situation not contemplated");
		if (obj1 == obj2)
			return;
		if (!obj1.equals(obj2))
			throw new RuntimeException("Constraint situation not contemplated");
	}
}
