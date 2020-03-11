package upv.slicing.edg.visitor;

public interface Visitable {
	<R, A> R accept(GenericVisitor<R, A> visitor, A argument);

	<A> void accept(VoidVisitor<A> visitor, A argument);
}
