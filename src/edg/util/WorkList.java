package edg.util;

import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import misc.util.Chronometer;

public class WorkList
{
	private enum Type { Done, Pending }
	private final int doneGroupSize = 1;
	private final int pendingGroupSize = 1;
	private final Map<Integer, Map<Integer, List<Work>>> done = new Hashtable<Integer, Map<Integer, List<Work>>>();
	private final Map<Integer, Map<Integer, List<Work>>> pending = new Hashtable<Integer, Map<Integer, List<Work>>>();

	public WorkList()
	{
		
	}
	public WorkList(List<Work> works)
	{
		for (Work work : works)
			this.add(work);
	}

	public boolean hasMore()
	{
		return !this.pending.isEmpty();
	}
	public Work next()
	{
		final int groupId = this.pending.keySet().iterator().next();
		final Map<Integer, List<Work>> worksMap = this.pending.get(groupId);
		final int groupId2 = worksMap.keySet().iterator().next();
		final List<Work> works = worksMap.get(groupId2);
		final Work nextWork = works.get(0);

		this.remove(Type.Pending, nextWork);

		return nextWork;
	}
	public void done(Work work)
	{
		this.add(Type.Done, work);
	}
	public List<Work> getDone(int currentNodeId)
	{
		return this.getWorks(Type.Done, currentNodeId);
	}

	public void add(Work work)
	{
		this.add(Type.Pending, work);
	}
	public void repending(List<Work> works)
	{
		for (Work work : works)
		{
			this.remove(Type.Done, work);
			this.add(Type.Pending, work);
		}
	}

	public boolean contains(Work work)
	{
// TODO Arreglar
Chronometer.start("1.4.3.1.1 - Done");
		final boolean done = this.isDone(work);
Chronometer.finish("1.4.3.1.1 - Done");
if (done)
return true;
Chronometer.start("1.4.3.1.2 - Pending");
		final boolean pending = this.isPending(work);
Chronometer.finish("1.4.3.1.2 - Pending");

		return done || pending;
//		return this.isDone(work) || this.isPending(work);
	}
	private boolean isDone(Work work)
	{
		final List<Work> works = this.getWorks(Type.Done, work);

		if (works == null)
			return false;
		return works.contains(work);
	}
	private boolean isPending(Work work)
	{
		final List<Work> works = this.getWorks(Type.Pending, work);

		if (works == null)
			return false;
		return works.contains(work);
	}

	public List<Work> toList()
	{
		final List<Work> works = new LinkedList<Work>();

		for (Map<Integer, List<Work>> doneMap : this.done.values())
			for (List<Work> doneList : doneMap.values())
				works.addAll(doneList);
		for (Map<Integer, List<Work>> pendingMap : this.pending.values())
			for (List<Work> pendingList : pendingMap.values())
				works.addAll(pendingList);

		return works;
	}

	private void add(Type type, Work work)
	{
		final Map<Integer, Map<Integer, List<Work>>> works = this.getWorks(type);
		final int groupId = this.getGroupId(type, work);
		Map<Integer, List<Work>> workMap = works.get(groupId);

		if (workMap == null)
		{
			workMap = new Hashtable<Integer, List<Work>>();
			works.put(groupId, workMap);
		}

		final int hashCode = this.getHashCode(work);
		List<Work> workList = workMap.get(hashCode);

		if (workList == null)
		{
			workList = new LinkedList<Work>();
			workMap.put(hashCode, workList);
		}

		workList.add(work);
	}
	private void remove(Type type, Work work)
	{
		final Map<Integer, Map<Integer, List<Work>>> works = this.getWorks(type);
		final int groupId = this.getGroupId(type, work);
		final Map<Integer, List<Work>> workMap = works.get(groupId);
		if (workMap == null)
			return;

		final int hashCode = this.getHashCode(work);
		final List<Work> workList = workMap.get(hashCode);
		if (workList == null)
			return;

		workList.remove(work);
		if (!workList.isEmpty())
			return;
		workMap.remove(hashCode);
		if (!workMap.isEmpty())
			return;
		works.remove(groupId);
	}

	private List<Work> getWorks(Type type, int id)
	{
		final Map<Integer, Map<Integer, List<Work>>> works = this.getWorks(type);
		final int groupId = this.getGroupId(type, id);
		final Map<Integer, List<Work>> workMap = works.get(groupId);
		final List<Work> typeWorks = new LinkedList<Work>();

		if (workMap == null)
			return typeWorks;
		for (List<Work> workList : workMap.values())
			for (Work work : workList)
				if (work.getCurrentNode().getData().getId() == id)
					typeWorks.add(work);

		return typeWorks;
	}
	private List<Work> getWorks(Type type, Work work)
	{
		final int groupId = this.getGroupId(type, work);
		final Map<Integer, Map<Integer, List<Work>>> works = this.getWorks(type);
		final Map<Integer, List<Work>> worksMap = works.get(groupId);
		if (worksMap == null)
			return new LinkedList<Work>();

		final int hashCode = this.getHashCode(work);
		final List<Work> workList = worksMap.get(hashCode);
		if (workList == null)
			return new LinkedList<Work>();

		return workList;
	}
	private Map<Integer, Map<Integer, List<Work>>> getWorks(Type type)
	{
		switch (type)
		{
			case Done:
				return this.done;
			case Pending:
				return this.pending;
			default:
				throw new RuntimeException("Type not contemplated yet: " + type);
		}
	}
	private int getGroupId(Type type, Work work)
	{
		final int id = work.getCurrentNode().getData().getId();

		return this.getGroupId(type, id);
	}
	private int getGroupId(Type type, int id)
	{
		switch (type)
		{
			case Done:
				return id - (id % this.doneGroupSize);
			case Pending:
				return id - (id % this.pendingGroupSize);
			default:
				throw new RuntimeException("Type not contemplated yet: " + type);
		}
	}
	private int getHashCode(Work work)
	{
		if (work.getInitialNode() == null)
			return work.getConstraints().toString().hashCode();
		return ("id" + work.getInitialNode().getData().getId() + "c" + work.getConstraints().toString()).hashCode();
	}
}