package edg.util;

import java.util.LinkedList;
import java.util.List;

public class WorkList3
{
	private final List<Work> done = new LinkedList<Work>();
	private final List<Work> pendings = new LinkedList<Work>();

	public WorkList3()
	{
		
	}
	public WorkList3(List<Work> works)
	{
		this.pendings.addAll(works);
	}

	public boolean hasMore()
	{
		return !this.pendings.isEmpty();
	}
	public Work next()
	{
		return this.pendings.remove(0);
	}
	public void done(Work work)
	{
		this.done.add(work);
	}
	public List<Work> getDone(int currentNodeId)
	{
		final List<Work> done = new LinkedList<Work>();

		for (Work workDone : this.done)
			if (workDone.getCurrentNode().getData().getId() == currentNodeId)
				done.add(workDone);

		return done;
	}

	public void add(Work work)
	{
		this.pendings.add(work);
	}
	public void repending(List<Work> works)
	{
		this.done.removeAll(works);
		this.pendings.addAll(works);
	}

	public boolean contains(Work work)
	{
		return this.done.contains(work) || this.pendings.contains(work);
	}

	public List<Work> toList()
	{
		final List<Work> works = new LinkedList<Work>();

		for (Work work : this.pendings)
			works.add(work);

		return works;
	}
}