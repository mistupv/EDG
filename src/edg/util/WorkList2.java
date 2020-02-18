package edg.util;

import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class WorkList2
{
	private final Map<Integer, List<Work>> done = new Hashtable<Integer, List<Work>>();
	private final List<Work> pending = new LinkedList<Work>();

	public WorkList2()
	{
		
	}
	public WorkList2(List<Work> works)
	{
		this.pending.addAll(works);
	}

	public boolean hasMore()
	{
		return !this.pending.isEmpty();
	}
	public Work next()
	{
		return this.pending.remove(0);
	}
	public void done(Work work)
	{
		final int id = work.getCurrentNode().getData().getId();
		List<Work> works = this.done.get(id);

		if (works == null)
		{
			works = new LinkedList<Work>();
			this.done.put(id, works);
		}
		works.add(work);
	}
	public List<Work> getDone(int currentNodeId)
	{
		final List<Work> works = this.done.get(currentNodeId);

		return works == null ? new LinkedList<Work>() : works;
	}

	public void add(Work work)
	{
		this.pending.add(work);
	}
	public void repending(List<Work> works)
	{
		for (Work work : works)
		{
			final int id = work.getCurrentNode().getData().getId();
			final List<Work> done = this.done.get(id);

			if (done != null)
				done.remove(work);
			this.pending.add(work);
		}
	}

	public boolean contains(Work work)
	{
		return this.isDone(work) || this.pending.contains(work);
	}
	private boolean isDone(Work work)
	{
		final int id = work.getCurrentNode().getData().getId();
		final List<Work> works = this.done.get(id);

		if (works == null)
			return false;
		return works.contains(work);
	}

	public List<Work> toList()
	{
		final List<Work> works = new LinkedList<Work>();

		for (List<Work> doneWorks : this.done.values())
			for (Work work : doneWorks)
				works.add(work);
		for (Work work : this.pending)
			works.add(work);

		return works;
	}
}