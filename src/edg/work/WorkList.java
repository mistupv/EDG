package edg.work;

import java.util.Collection;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import edg.graph.Node;

public class WorkList
{
	private final Map<String, Set<Work>> doneWorks = new Hashtable<String, Set<Work>>();
	private final Map<String, Set<Work>> pendingWorks = new Hashtable<String, Set<Work>>();

	public WorkList(Work... works)
	{
		this.pendAll(works);
	}
	public WorkList(Iterable<Work> works)
	{
		this.pendAll(works);
	}

	public Set<Node> getPendingNodes()
	{
		return this.getNodes(this.pendingWorks);
	}
	public Set<Node> getDoneNodes()
	{
		return this.getNodes(this.doneWorks);
	}
	private Set<Node> getNodes(Map<String, Set<Work>> workMap)
	{
		final Collection<Set<Work>> workCollection = workMap.values();
		final Set<Node> nodes = new HashSet<Node>();

		for (Set<Work> workSet : workCollection)
			for (Work work : workSet)
				if (work instanceof NodeWork)
				{
					final NodeWork nodeWork = (NodeWork) work;
					nodes.add(nodeWork.getCurrentNode());
					break;
				}

		return nodes;
	}
	public Set<Work> getPendingWorks(String key)
	{
		return this.pendingWorks.get(key);
	}
	public Set<Work> getDoneWorks(String key)
	{
		return this.doneWorks.get(key);
	}
	private Set<Work> createPendingWorks(String key)
	{
		Set<Work> pendingWorks = this.getPendingWorks(key);

		if (pendingWorks == null)
		{
			pendingWorks = new HashSet<Work>();
			this.pendingWorks.put(key, pendingWorks);
		}

		return pendingWorks;
	}
	private Set<Work> createDoneWorks(String key)
	{
		Set<Work> doneWorks = this.getDoneWorks(key);

		if (doneWorks == null)
		{
			doneWorks = new HashSet<Work>();
			this.doneWorks.put(key, doneWorks);
		}

		return doneWorks;
	}

	public boolean hasMore()
	{
		return !this.pendingWorks.isEmpty();
	}
	public Work next()
	{
		final Iterator<Entry<String, Set<Work>>> iterator = this.pendingWorks.entrySet().iterator();
		final Entry<String, Set<Work>> entry = iterator.next();
		final Set<Work> pendingWorks = entry.getValue();
		final Iterator<Work> workIterator = pendingWorks.iterator();
		final Work work = workIterator.next();			
		
		workIterator.remove();
		if (pendingWorks.isEmpty())
			iterator.remove();

		return work;
	}
	public void done(Work work)
	{
		final String key = work.getId();
		final Set<Work> doneWorks = this.createDoneWorks(key);

		doneWorks.add(work);
	}

	public void pendAll(Work... works)
	{
		for (Work work : works)
			this.pend(work);
	}
	public void pendAll(Iterable<Work> works)
	{
		for (Work work : works)
			this.pend(work);
	}
	public void pend(Work work)
	{
		if (this.contains(work))
			return;

		final String key = work.getId();
		final Set<Work> pendingWorks = this.createPendingWorks(key);

		pendingWorks.add(work);
	}
	public void repend()
	{
		final Set<String> keys0 = this.doneWorks.keySet();
		final Set<String> keys = new HashSet<String>(keys0);

		for (String key : keys)
			this.repend(key);
	}
//private long rependedTimes = 0;
//private long rependedWorks = 0;
	public void repend(String key)
	{
// TODO Borrame
//this.rependedTimes++;
//System.out.println("Repending " + key);
		final Set<Work> doneWorks = this.doneWorks.get(key);
		if (doneWorks == null)
			return;

//this.rependedWorks += doneWorks.size();
//System.out.println("Repended so far: " + this.rependedWorks + " [" + doneWorks.size() + "]");
		final Set<Work> pendingWorks = this.createPendingWorks(key);
		pendingWorks.addAll(doneWorks);
		this.doneWorks.remove(key);
	}
	
	public boolean contains(Work work)
	{
		final String key = work.getId();

		final Set<Work> doneWorks = this.getDoneWorks(key);
		if (doneWorks != null && doneWorks.contains(work))
			return true;

		final Set<Work> pendingWorks = this.getPendingWorks(key);
		if (pendingWorks != null && pendingWorks.contains(work))
			return true;

		return false;
	}	
}