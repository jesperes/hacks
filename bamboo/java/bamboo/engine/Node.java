package bamboo.engine;

import java.io.File;
import java.util.ArrayList;

public class Node {

	ArrayList<Node> deps = new ArrayList<Node>();
	
	File file;
	NodeSignature sig;
	boolean dirty;
	
	Node(String filename)
	{
		bind(new File(filename));
		sig = new NodeSignature(this);
	}
	
	Node(File file)
	{
		bind(file);
		sig = new NodeSignature(this);
	}

	void populateDependencies(int level, int numDeps, String nodeName)
	{
		if (level <= 0)
			return;
		else {
			for (int i = 0; i < numDeps; i++) {
				String s = nodeName + "-" + i;
				Node dep = new Node(s);
				dep.populateDependencies(level - 1, numDeps, s);
				addDependency(dep);
			}
		}
	}
	

	/** Bind this node to a specific target. */
	void bind(File file)
	{
		this.file = file;
	}

	public String toString()
	{
		return file.getAbsolutePath();
	}
	
	void addDependency(Node dep)
	{
		 deps.add(dep);
	}
	
	/**
	 * Update this node's dirty mark. Returns the total number of nodes
	 * checked.
	 */
	int updateDirtyMark()
	{
		int numNodes = 1;
		
		dirty = false;
		
		if (!file.exists())
			dirty = true;
		
		for (Node dep: deps)
		{
			numNodes += dep.updateDirtyMark();
			
			if (dep.dirty)
				dirty = true;
			else if (dep.moreRecentThan(this))
				dirty = true;

		}
		
		return numNodes;
	}
	
	
	/**
	 * Returns true/false to indicate if this node is "more recent" than
	 * another node.
	 */
	private boolean moreRecentThan(Node other) 
	{
		return sig.getTimeStamp() > other.sig.getTimeStamp();  
	}

	String formatTree()
	{
		updateDirtyMark();
		
		StringBuffer s = new StringBuffer();
		formatTree(s, 0);
		return s.toString();
	}
	
	void formatTree(StringBuffer s, int level)
	{
		for (int i = 0; i < level*2; i++)
			s.append(" ");
		
		s.append(this + " (" + sig.getTimeStampAsString() + ") ");
		s.append(dirty ? "(*)" : "");
		s.append("\n");
		
		for (Node n: deps)
		{
			n.formatTree(s, level + 1);
		}
	}	
}
