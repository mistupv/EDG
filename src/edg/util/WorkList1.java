package edg.util;

import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class WorkList1
{
	private final int doneGroupSize = 1;
	private final int pendingGroupSize = 1;
	private final Map<Integer, List<Work>> done = new Hashtable<Integer, List<Work>>();
	private final Map<Integer, List<Work>> pending = new Hashtable<Integer, List<Work>>();

	public WorkList1()
	{
		
	}
	public WorkList1(List<Work> works)
	{
		for (Work work : works)
			this.add(work);
	}

	private int getDoneGroupId(int id)
	{
		return id - (id % this.doneGroupSize);
	}
	private int getPendingGroupId(int id)
	{
		return id - (id % this.pendingGroupSize);
	}

	public boolean hasMore()
	{
		return !this.pending.isEmpty();
	}
	public Work next()
	{
		final Set<Integer> keySet = this.pending.keySet();
		final int groupId = keySet.iterator().next();
		final List<Work> works = this.pending.get(groupId);
		final Work nextWork = works.get(0);

		this.removePending(nextWork);

		return nextWork;
	}
	public void done(Work work)
	{
		this.addDone(work);
	}
	public List<Work> getDone(int currentNodeId)
	{
		final int groupId = this.getDoneGroupId(currentNodeId);
		final List<Work> works = this.done.get(groupId);
		final List<Work> clonedWorks = new LinkedList<Work>();

		if (works == null)
			return clonedWorks;
		for (Work work : works)
			if (work.getCurrentNode().getData().getId() == currentNodeId)
				clonedWorks.add(work);

		return clonedWorks;
	}

	public void add(Work work)
	{
		this.addPending(work);
	}
	public void repending(List<Work> works)
	{
		for (Work work : works)
		{
			this.removeDone(work);
			this.addPending(work);
		}
	}

	public boolean contains(Work work)
	{
		return this.isDone(work) || this.isPending(work);
	}
	private boolean isDone(Work work)
	{
		final int id = work.getCurrentNode().getData().getId();
		final int groupId = this.getDoneGroupId(id);
		final List<Work> works = this.done.get(groupId);

		if (works == null)
			return false;
		return works.contains(work);
	}
	private boolean isPending(Work work)
	{
		final int id = work.getCurrentNode().getData().getId();
		final int groupId = this.getPendingGroupId(id);
		final List<Work> works = this.pending.get(groupId);

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
		for (List<Work> pendingWorks : this.pending.values())
			for (Work work : pendingWorks)
				works.add(work);

		return works;
	}

	private void addPending(Work work)
	{
		final int id = work.getCurrentNode().getData().getId();
		final int groupId = this.getPendingGroupId(id);
		List<Work> pending = this.pending.get(groupId);

		if (pending == null)
		{
			pending = new LinkedList<Work>();
			this.pending.put(groupId, pending);
		}
		pending.add(work);
	}
	private void removePending(Work work)
	{
		final int id = work.getCurrentNode().getData().getId();
		final int groupId = this.getPendingGroupId(id);
		final List<Work> pending = this.pending.get(groupId);

		if (pending != null)
		{
			pending.remove(work);
			if (pending.isEmpty())
				this.pending.remove(groupId);
		}
	}
	private void addDone(Work work)
	{
		final int id = work.getCurrentNode().getData().getId();
		final int groupId = this.getDoneGroupId(id);
		List<Work> done = this.done.get(groupId);

		if (done == null)
		{
			done = new LinkedList<Work>();
			this.done.put(groupId, done);
		}
		done.add(work);
	}
	private void removeDone(Work work)
	{
		final int id = work.getCurrentNode().getData().getId();
		final int groupId = this.getDoneGroupId(id);
		final List<Work> done = this.done.get(groupId);

		if (done != null)
		{
			done.remove(work);
			if (done.isEmpty())
				this.done.remove(groupId);
		}
	}
}