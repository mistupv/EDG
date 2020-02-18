package eknife.edg.slicingAlgorithm;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import eknife.edg.EDG;
import eknife.edg.EdgeInfo;
import eknife.edg.Node;
import eknife.edg.traverser.GraphTraverser;

public class SlicingORBS implements SlicingAlgorithm{
	private final EDG graph;
	private List<Node> supressed = new LinkedList<Node>(); // Lista de nodos que ya hemos intentado eliminar
	private List<Node> processed = new LinkedList<Node>(); // Lista de nodos cuyos hijos ya han sido eliminados y probados
	private List<Node> unnecessary = new LinkedList<Node>(); //Lista de nodos que tras evaluarlos son no necesarios
	private Node currentNode;
	private List<Node> nodes;
	
	public SlicingORBS(EDG graph)
	{
		this.graph = graph;
		this.nodes = this.graph.getNodes();
		this.currentNode = this.nodes.get(0);
		this.supressed.add(this.currentNode);
		this.appendMethods();

	}
	public List<Node> slice(Node node)
	{
		return this.ORBS1();
	}
	public void overwriteSlice(Node deleted)
	{
		this.processed.add(this.currentNode);
		this.currentNode = GraphTraverser.getParent(this.currentNode, EdgeInfo.Type.Control);
		this.nodes.remove(deleted);
	}
	public Node getCurrent()
	{
		return this.currentNode;
	}
	public List<Node> getUnnecessary()
	{
		return this.unnecessary;
	}
	
	public List<Node> ORBS1() // Eliminando nodos de 1 en 1
	{
		List<Node> slice = new LinkedList<Node>(this.nodes);
		Node destroyed = null;
		do
		{
			if (this.processed.contains(this.currentNode)) //Si ya hemos eliminado todos sus hijos, subimos al padre
			{
				if(GraphTraverser.getParent(this.currentNode, EdgeInfo.Type.Control) == null)
					return null;
				else
					this.currentNode = GraphTraverser.getParent(this.currentNode, EdgeInfo.Type.Control);
			}
			else // Si no:
			{
				if (this.supressed.contains(this.currentNode))//eliminamos alguno de sus hijos que aun no se haya eliminado
				{
					List<Node> children = GraphTraverser.getChildren(this.currentNode, EdgeInfo.Type.Control);
					Node Father = this.currentNode;
					Iterator<Node> iterator = children.iterator();
					boolean exit = false;
					boolean allProcessed = true;
					while (iterator.hasNext() && !exit){
						Node child = iterator.next();
						if(!this.supressed.contains(child)) 
						{
							destroyed = child; 
							allProcessed = false;
							exit = true;
							this.supressed.add(child);
							if(GraphTraverser.getChildren(child, EdgeInfo.Type.Control).size() == 0)
								this.processed.add(child);
							else
								this.currentNode = child;
						}
						else
						{
							if(!this.processed.contains(child))
							{
								this.currentNode = child;
								allProcessed = false;
								exit = true;
							}
						}
					}
					if(allProcessed)
						this.processed.add(Father);
				}
				else
				{
					destroyed = this.currentNode; //lo eliminamos si aun no se ha intentado
					this.supressed.add(destroyed);
				}
			}
			if(destroyed != null){
				String nodeName = destroyed.getName();
				switch(destroyed.getName())
				{
					case "(var)\\n_":
					case "(atom)\\nundef":
					case "return":
					case "body":
						destroyed = null;
						break;
					default: 
						if(nodeName.length()>=9 && nodeName.substring(0,8).equals("(guards)"))
							destroyed = null;
						break;
				}
			}
			
		}while(destroyed == null); 
		this.removeDescendants(slice, destroyed);
		return slice;
	}
	
	public List<Node> ORBS2() // Eliminando nodos de 2 en 2
	{
		return null;
	}
	private void removeDescendants(List<Node> slice, Node node) //Eliminar un nodo y sus descendientes
	{
		slice.remove(node);
		List<Node> children = GraphTraverser.getChildren(node, EdgeInfo.Type.Control);
		for (Node child : children)
			this.removeDescendants(slice, child);
	}
	public void addUnnecessary(Node node)
	{
		this.unnecessary.add(node);
		List<Node> children = new LinkedList<Node>();
		children = GraphTraverser.getChildren(node, EdgeInfo.Type.Control);
		Iterator<Node> iterator = children.iterator();
		
		while (iterator.hasNext())
		{
			Node nodeHijo = iterator.next();
			addUnnecessary(nodeHijo);
		}
	}
	private void appendMethods() // Comprueba si pertenece a los metodos añadidos al final del codigo para imprimir en fichero
	{
		int N = this.nodes.size();
		for(int index=1;index<N;index++)
		{
			Node node = this.nodes.get(index);
			if(node.getName().equals("function\\nslice/1") || node.getName().equals("function\\nslice_void/1"))
			{
				this.processed.add(node);
				this.supressed.add(node);
			}
			else if(node.getName().equals("(atom)\\nslice") || node.getName().equals("(atom)\\nslice_void"))
			{
				this.processed.add(GraphTraverser.getParent(node, EdgeInfo.Type.Control));
				this.supressed.add(GraphTraverser.getParent(node, EdgeInfo.Type.Control));

			}
		}	
	}
}
