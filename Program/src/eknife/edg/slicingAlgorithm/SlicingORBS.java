package eknife.edg.slicingAlgorithm;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import eknife.edg.EDG;
import eknife.edg.EdgeInfo;
import eknife.edg.Node;
import eknife.edg.NodeInfo;
import eknife.edg.traverser.GraphTraverser;

public class SlicingORBS implements SlicingAlgorithm{
	private final EDG graph;
	private List<Node> supressed = new LinkedList<Node>(); // Lista de nodos que ya hemos intentado eliminar
	private List<Node> processed = new LinkedList<Node>(); // Lista de nodos cuyos hijos ya han sido eliminados y probados
	private List<Node> unnecessary = new LinkedList<Node>(); //Lista de nodos que tras evaluarlos son no necesarios
	private Node currentNode;
	private List<Node> nodes;
	private int eliminations = 0;
	
	//------Añadido para ORBS2------
	private Node pairedNode;
	private List<Node> pairedProcessed = new LinkedList<Node>();
	private List<Node> pairedSupressed = new LinkedList<Node>();
	//------------------------------
	
	//------Añadido para ORBS2------
	private Node tripleNode;
	private List<Node> tripleProcessed = new LinkedList<Node>();
	private List<Node> tripleSupressed = new LinkedList<Node>();
	//------------------------------
	
	public List<Node> getNodes(){
		return this.nodes;
	}
	
	public SlicingORBS(EDG graph)
	{
		this.graph = graph;
		this.nodes = this.graph.getNodes();
		this.currentNode = this.nodes.get(0);
		this.supressed.add(this.currentNode);
		this.appendMethods();
		
		this.pairedNode = this.nodes.get(0);
		this.pairedSupressed.add(this.pairedNode);
		this.appendMethodsPaired();
		
		this.tripleNode = this.nodes.get(0);
		this.tripleSupressed.add(this.tripleNode);
		this.appendMethodsTriple();
	}
	public List<Node> slice(Node node)
	{
		return this.ORBS1();
	}
	public void overwriteSlice(Node deleted)
	{
		this.processed.clear();
		this.supressed.clear();
		this.currentNode = this.nodes.get(0);
		this.supressed.add(this.currentNode);
		this.nodes.remove(deleted);
		this.appendMethods();
		
		//ORBS2
		if(this.pairedNode != null){
			this.nodes.remove(this.pairedNode);
			this.pairedProcessed.clear();
			this.pairedSupressed.clear();
			this.pairedNode = this.nodes.get(0);
			this.pairedSupressed.add(this.pairedNode);
			this.appendMethodsPaired();
		}
		
		//ORBS3
		if(this.tripleNode != null){
			this.nodes.remove(this.tripleNode);
			this.tripleProcessed.clear();
			this.tripleSupressed.clear();
			this.tripleNode = this.nodes.get(0);
			this.tripleSupressed.add(this.tripleNode);
			this.appendMethodsTriple();
		}
		this.eliminations++;
	}
	public Node getCurrent()
	{
		return this.currentNode;
	}
	public Node getPaired(){
		return this.pairedNode;
	}
	public Node getTriple(){
		return this.tripleNode;
	}
	public int getEliminations(){
		return this.eliminations;
	}
	public List<Node> getUnnecessary()
	{
		return this.unnecessary;
	}
	
	public List<Node> ORBS1() // Eliminando nodos de 1 en 1
	{
		this.pairedNode = null;
		this.tripleNode = null;
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
				if (destroyed.getData().getType() == NodeInfo.Type.ListComprehensionResult || this.unnecessary.contains(destroyed)){
					this.processed.add(destroyed);
					destroyed = null;
				}	
				else{
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
			}
			
		}while(destroyed == null); 
		this.removeUnnecessary(slice);
		this.removeDescendants(slice, destroyed);
		return slice;
	}
	
	public List<Node> ORBS2() // Eliminando nodos de 2 en 2
	{
		this.tripleNode = null;
		Node destroyed = null;
		if(!this.supressed.contains(this.currentNode)){
			List<Node> slice = ORBSPairMe();
			if (slice == null){ //Si devuelve null es que ya ha recorrido todo, reiniciamos para la siguiente iteracion
				this.supressed.add(this.currentNode);		
				this.pairedProcessed.clear();
				this.pairedSupressed.clear();
				this.pairedNode = this.nodes.get(0);
				this.appendMethodsPaired();
				if(GraphTraverser.getChildren(this.currentNode, EdgeInfo.Type.Control).size() == 0)
					this.processed.add(this.currentNode);
				return ORBS2();
			}
			else
				return slice;
			}
		else{
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
					}
				}
				if(destroyed != null){
					if(this.unnecessary.contains(destroyed)){
						this.processed.add(destroyed);
						if(!this.supressed.contains(destroyed))
							this.supressed.add(destroyed);
						destroyed=null;
					}
					else{
						String nodeName = destroyed.getName();
						switch(destroyed.getName())
						{
							case "(var)\\n_":
							case "(atom)\\nundef":
							case "return":
							case "body":
								this.supressed.add(destroyed);
								destroyed = null;
								break;
							default: 
								if(nodeName.length()>=9 && nodeName.substring(0,8).equals("(guards)")){
									this.supressed.add(destroyed);
									destroyed = null;
								}
								break;
						}
					}
				}
				
			}while(destroyed == null);
			this.pairedSupressed.add(this.currentNode);
			this.pairedProcessed.add(this.currentNode);
			return ORBS2();
		}
	}
	private List<Node> ORBSPairMe() {
		List<Node> slice = new LinkedList<Node>(this.nodes);
		Node destroyed = null;
		do
		{
			if (this.pairedProcessed.contains(this.pairedNode)) //Si ya hemos eliminado todos sus hijos, subimos al padre
			{
				if(GraphTraverser.getParent(this.pairedNode, EdgeInfo.Type.Control) == null){
					return null;
				}
				else
					this.pairedNode = GraphTraverser.getParent(this.pairedNode, EdgeInfo.Type.Control);
			}
			else{
				if(this.pairedSupressed.contains(this.pairedNode))
				{
					List<Node> children = GraphTraverser.getChildren(this.pairedNode, EdgeInfo.Type.Control);
					Node Father = this.pairedNode;
					Iterator<Node> iterator = children.iterator();
					boolean exit = false;
					boolean allProcessed = true;
					while (iterator.hasNext() && !exit){
						Node child = iterator.next();
						if(!this.pairedSupressed.contains(child)) 
						{
							destroyed = child; 
							allProcessed = false;
							exit = true;
							this.pairedSupressed.add(child);
							if(GraphTraverser.getChildren(child, EdgeInfo.Type.Control).size() == 0)
								this.pairedProcessed.add(child);
							this.pairedNode = child;
						}
						else
						{
							if(!this.pairedProcessed.contains(child))
							{
								this.pairedNode = child;
								allProcessed = false;
								exit = true;
							}
						}
					}
					if(allProcessed)
						this.pairedProcessed.add(Father);
				}
				else
				{
					destroyed = this.pairedNode; 
					this.pairedSupressed.add(destroyed);
				}
			}
			if(isFather(destroyed,this.currentNode) || this.supressed.contains(destroyed)){ //Si es padre o ya se han probado todas sus combinaciones no se elimina, pasamos al siguiente
				this.pairedSupressed.add(destroyed);
				destroyed = null;
			}
			if(destroyed != null){ //Si es innecesario pasamos de el
				if(this.unnecessary.contains(destroyed)){
					this.pairedProcessed.add(destroyed);
					destroyed=null;
				}
				else{
					String nodeName = destroyed.getName();
					switch(destroyed.getName())
					{
						case "(var)\\n_":
						case "(atom)\\nundef":
						case "return":
						case "body":
							this.pairedSupressed.add(destroyed);
							destroyed = null;
							break;
						default: 
							if(nodeName.length()>=9 && nodeName.substring(0,8).equals("(guards)")){
								this.pairedSupressed.add(destroyed);
								destroyed = null;
							}
							break;
					}
				}	
			}
		}while(destroyed == null);
		this.removeUnnecessary(slice);
		this.removeDescendants(slice, this.currentNode);
		this.removeDescendants(slice, destroyed);
		return slice;
	}
	
	public List<Node> ORBS3() // Eliminando nodos de 3 en 3
	{
		Node destroyed = null;
		if(!this.supressed.contains(this.currentNode)){
			List<Node> slice = ORBSecondNode();
			if (slice == null){ //Si devuelve null es que ya ha recorrido todo, reiniciamos para la siguiente iteracion
				this.supressed.add(this.currentNode);		
				this.pairedProcessed.clear();
				this.pairedSupressed.clear();
				this.pairedNode = this.nodes.get(0);
				this.pairedSupressed.add(this.pairedNode);
				this.appendMethodsPaired();
				this.tripleProcessed.clear();
				this.tripleSupressed.clear();
				this.tripleNode = this.nodes.get(0);
				this.tripleSupressed.add(this.tripleNode);
				this.appendMethodsTriple();
				if(GraphTraverser.getChildren(this.currentNode, EdgeInfo.Type.Control).size() == 0)
					this.processed.add(this.currentNode);
				return ORBS3();
			}
			else
				return slice;
			}
		else{
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
					}
				}
				if(destroyed != null){
					if(this.unnecessary.contains(destroyed)){
						this.processed.add(destroyed);
						if(!this.supressed.contains(destroyed))
							this.supressed.add(destroyed);
						destroyed=null;
					}
					else{
						String nodeName = destroyed.getName();
						switch(destroyed.getName())
						{
							case "(var)\\n_":
							case "(atom)\\nundef":
							case "return":
							case "body":
								this.supressed.add(destroyed);
								destroyed = null;
								break;
							default: 
								if(nodeName.length()>=9 && nodeName.substring(0,8).equals("(guards)")){
									this.supressed.add(destroyed);
									destroyed = null;
								}
								break;
						}
					}
				}
				
			}while(destroyed == null);
			this.pairedSupressed.add(this.currentNode);
			this.pairedProcessed.add(this.currentNode);
			this.tripleSupressed.add(this.currentNode);
			this.tripleProcessed.add(this.currentNode);
			return ORBS3();
		}
	}
	private List<Node> ORBSecondNode() //Pending
	{
		Node destroyed = null;
		if(!this.pairedSupressed.contains(this.pairedNode)){
			List<Node> slice = ORBSTriple();
			if (slice == null){ //Si devuelve null es que ya ha recorrido todo, reiniciamos para la siguiente iteracion
				this.pairedSupressed.add(this.pairedNode);		
				this.tripleProcessed.clear();
				this.tripleSupressed.clear();
				this.tripleNode = this.nodes.get(0);
				this.tripleSupressed.add(this.tripleNode);
				this.tripleSupressed.add(this.currentNode);
				this.tripleProcessed.add(this.currentNode);
				this.appendMethodsTriple();
				if(GraphTraverser.getChildren(this.pairedNode, EdgeInfo.Type.Control).size() == 0)
					this.pairedProcessed.add(this.pairedNode);
				return ORBSecondNode();
			}
			else
				return slice;
			}
		else{
			do
			{
				if (this.pairedProcessed.contains(this.pairedNode)) //Si ya hemos eliminado todos sus hijos, subimos al padre
				{
					if(GraphTraverser.getParent(this.pairedNode, EdgeInfo.Type.Control) == null)
						return null;
					else
						this.pairedNode = GraphTraverser.getParent(this.pairedNode, EdgeInfo.Type.Control);
				}
				else // Si no:
				{
					if (this.pairedSupressed.contains(this.pairedNode))//eliminamos alguno de sus hijos que aun no se haya eliminado
					{
						List<Node> children = GraphTraverser.getChildren(this.pairedNode, EdgeInfo.Type.Control);
						Node Father = this.pairedNode;
						Iterator<Node> iterator = children.iterator();
						boolean exit = false;
						boolean allProcessed = true;
						while (iterator.hasNext() && !exit){
							Node child = iterator.next();
							if(!this.pairedSupressed.contains(child)) 
							{
								destroyed = child; 
								allProcessed = false;
								exit = true;
								this.pairedNode = child;
							}
							else
							{
								if(!this.pairedProcessed.contains(child))
								{
									this.pairedNode = child;
									allProcessed = false;
									exit = true;
								}
							}
						}
						if(allProcessed)
							this.pairedProcessed.add(Father);
					}
					else
					{
						destroyed = this.pairedNode; //lo eliminamos si aun no se ha intentado
					}
				}
				if(isFather(destroyed,this.currentNode) || this.supressed.contains(destroyed)){ //Si es padre o ya se han probado todas sus combinaciones no se elimina, pasamos al siguiente
					this.pairedSupressed.add(destroyed);
					destroyed = null;
				}
				if(destroyed != null){
					if(this.unnecessary.contains(destroyed)){
						this.pairedProcessed.add(destroyed);
						if(!this.pairedSupressed.contains(destroyed))
							this.pairedSupressed.add(destroyed);
						destroyed=null;
					}
					else{
						String nodeName = destroyed.getName();
						switch(destroyed.getName())
						{
							case "(var)\\n_":
							case "(atom)\\nundef":
							case "return":
							case "body":
								this.pairedSupressed.add(destroyed);
								destroyed = null;
								break;
							default: 
								if(nodeName.length()>=9 && nodeName.substring(0,8).equals("(guards)")){
									this.pairedSupressed.add(destroyed);
									destroyed = null;
								}
								break;
						}
					}
				}
				
			}while(destroyed == null);
			this.tripleSupressed.add(this.pairedNode);
			this.tripleProcessed.add(this.pairedNode);
			return ORBSecondNode();
		}
	}
	private List<Node> ORBSTriple() //Pending
	{
		List<Node> slice = new LinkedList<Node>(this.nodes);
		Node destroyed = null;
		do
		{
			if (this.tripleProcessed.contains(this.tripleNode)) //Si ya hemos eliminado todos sus hijos, subimos al padre
			{
				if(GraphTraverser.getParent(this.tripleNode, EdgeInfo.Type.Control) == null){
					return null;
				}
				else
					this.tripleNode = GraphTraverser.getParent(this.tripleNode, EdgeInfo.Type.Control);
			}
			else{
				if(this.tripleSupressed.contains(this.tripleNode))
				{
					
					List<Node> children = GraphTraverser.getChildren(this.tripleNode, EdgeInfo.Type.Control);
					Node Father = this.tripleNode;
					Iterator<Node> iterator = children.iterator();
					boolean exit = false;
					boolean allProcessed = true;
					while (iterator.hasNext() && !exit){
						Node child = iterator.next();
						if(!this.tripleSupressed.contains(child)) 
						{
							destroyed = child; 
							allProcessed = false;
							exit = true;
							this.tripleSupressed.add(child);
							if(GraphTraverser.getChildren(child, EdgeInfo.Type.Control).size() == 0)
								this.tripleProcessed.add(child);
							this.tripleNode = child;
						}
						else
						{
							if(!this.tripleProcessed.contains(child))
							{
								this.tripleNode = child;
								allProcessed = false;
								exit = true;
							}
						}
					}
					if(allProcessed)
						this.tripleProcessed.add(Father);
				}
				else
				{
					destroyed = this.tripleNode; 
					this.tripleSupressed.add(destroyed);
				}
			}
			if(isFather(destroyed,this.currentNode) || isFather(destroyed,this.pairedNode) || this.supressed.contains(destroyed) || this.pairedSupressed.contains(destroyed)){ //Si es padre o ya se han probado todas sus combinaciones no se elimina, pasamos al siguiente
				if(!this.tripleSupressed.contains(destroyed))
					this.tripleSupressed.add(destroyed);
				destroyed = null;
			}	
			if(destroyed != null){ //Si es innecesario pasamos de el
				if(this.unnecessary.contains(destroyed)){
					this.tripleProcessed.add(destroyed);
					destroyed=null;
				}
				else{
					String nodeName = destroyed.getName();
					switch(destroyed.getName())
					{
						case "(var)\\n_":
						case "(atom)\\nundef":
						case "return":
						case "body":
							this.tripleSupressed.add(destroyed);
							destroyed = null;
							break;
						default: 
							if(nodeName.length()>=9 && nodeName.substring(0,8).equals("(guards)")){
								this.tripleSupressed.add(destroyed);
								destroyed = null;
							}
							break;
					}
				}	
			}
		}while(destroyed == null);
		this.removeUnnecessary(slice);
		this.removeDescendants(slice, this.currentNode);
		this.removeDescendants(slice, this.pairedNode);
		this.removeDescendants(slice, destroyed);
		return slice;
	}
	
	private void removeUnnecessary(List<Node> slice)
	{
		for (Node unnecessaryNode : this.unnecessary)
			this.removeDescendants(slice, unnecessaryNode);
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
		if(!this.unnecessary.contains(node))
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
			else if((node.getName().length() >= 20 && node.getName().substring(0,19).equals("function\\nfunundef/")) || node.getName().equals("(atom)\\nfunundef"))
			{
				this.processed.add(node);
				this.supressed.add(node);
			}
		}	
	}
	private void appendMethodsPaired() // Comprueba si pertenece a los metodos añadidos al final del codigo para imprimir en fichero
	{
		int N = this.nodes.size();
		for(int index=1;index<N;index++)
		{
			Node node = this.nodes.get(index);
			if(node.getName().equals("function\\nslice/1") || node.getName().equals("function\\nslice_void/1"))
			{
				this.pairedProcessed.add(node);
				this.pairedSupressed.add(node);
			}
			else if(node.getName().equals("(atom)\\nslice") || node.getName().equals("(atom)\\nslice_void"))
			{
				this.pairedProcessed.add(GraphTraverser.getParent(node, EdgeInfo.Type.Control));
				this.pairedSupressed.add(GraphTraverser.getParent(node, EdgeInfo.Type.Control));
			}
			else if((node.getName().length() >= 20 && node.getName().substring(0,19).equals("function\\nfunundef/")) || node.getName().equals("(atom)\\nfunundef"))
			{
				this.pairedProcessed.add(node);
				this.pairedSupressed.add(node);
			}
		}	
	}
	private void appendMethodsTriple() // Comprueba si pertenece a los metodos añadidos al final del codigo para imprimir en fichero
	{
		int N = this.nodes.size();
		for(int index=1;index<N;index++)
		{
			Node node = this.nodes.get(index);
			if(node.getName().equals("function\\nslice/1") || node.getName().equals("function\\nslice_void/1"))
			{
				this.tripleProcessed.add(node);
				this.tripleSupressed.add(node);
			}
			else if(node.getName().equals("(atom)\\nslice") || node.getName().equals("(atom)\\nslice_void"))
			{
				this.tripleProcessed.add(GraphTraverser.getParent(node, EdgeInfo.Type.Control));
				this.tripleSupressed.add(GraphTraverser.getParent(node, EdgeInfo.Type.Control));
			}
			else if((node.getName().length() >= 20 && node.getName().substring(0,19).equals("function\\nfunundef/")) || node.getName().equals("(atom)\\nfunundef"))
			{
				this.tripleProcessed.add(node);
				this.tripleSupressed.add(node);
			}
		}	
	}
	private boolean isFather(Node node, Node son)
	{
		Node father = GraphTraverser.getParent(son, EdgeInfo.Type.Control);
		if (father == null)
			return false;
		else if (father.equals(node))
			return true;
		else
			return isFather(node, father);
	}
}
