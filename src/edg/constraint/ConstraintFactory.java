package edg.constraint;

import java.util.HashSet;
import java.util.Hashtable;
import java.util.Map;
import java.util.Set;

import edg.constraint.AccessConstraint.Operation;
import edg.slicing.Phase;

public class ConstraintFactory
{
	private static ConstraintFactory factory = new ConstraintFactory();
	public static ConstraintFactory getFactory()
	{
		return ConstraintFactory.factory;
	}

	private enum Type { DataConstructor, ListComprehension, List, AddNode, Asterisk, Empty, Phase }
	private Map<Type, Set<Constraint>> constraints = new Hashtable<Type, Set<Constraint>>();

	private ConstraintFactory()
	{
		for (Type type : Type.values())
			this.constraints.put(type, new HashSet<Constraint>());
		this.constraints.get(Type.Asterisk).add(AsteriskConstraint.getConstraint());
		this.constraints.get(Type.Empty).add(EmptyConstraint.getConstraint());
	}

	private Constraint getConstraint(Type type, Constraint constraint)
	{
		final Set<Constraint> constraints = this.constraints.get(type);

		for (Constraint oldConstraint : constraints)
			if (oldConstraint.equals(constraint))
				return oldConstraint;

		constraints.add(constraint);
		return constraint;
	}

	public DataConstructorConstraint getDataConstructorConstraint(Operation operation, String index)
	{
		final DataConstructorConstraint constraint = new DataConstructorConstraint(operation, index);
		return (DataConstructorConstraint) this.getConstraint(Type.DataConstructor, constraint);
	}
	public ListComprehensionConstraint getListComprehensionConstraint(Operation operation)
	{
		final ListComprehensionConstraint constraint = new ListComprehensionConstraint(operation);
		return (ListComprehensionConstraint) this.getConstraint(Type.ListComprehension, constraint);
	}
	public ListConstraint getListConstraint(Operation operation)
	{
		final ListConstraint constraint = new ListConstraint(operation);
		return (ListConstraint) this.getConstraint(Type.List, constraint);
	}
	public AddNodeConstraint getAddNodeConstraint(NodeConstraint nodeConstraint)
	{
		final AddNodeConstraint constraint = new AddNodeConstraint(nodeConstraint);
		return (AddNodeConstraint) this.getConstraint(Type.AddNode, constraint);
	}
	public AsteriskConstraint getAsteriskConstraint()
	{
		return (AsteriskConstraint) this.constraints.get(Type.Asterisk).iterator().next();
	}
	public EmptyConstraint getEmptyConstraint()
	{
		return (EmptyConstraint) this.constraints.get(Type.Empty).iterator().next();
	}
	public PhaseConstraint getPhaseConstraint(Phase... phases)
	{
		final PhaseConstraint constraint = new PhaseConstraint(phases);
		return (PhaseConstraint) this.getConstraint(Type.Phase, constraint);
	}
}