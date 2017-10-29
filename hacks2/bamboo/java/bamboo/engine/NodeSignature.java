package bamboo.engine;

import java.util.Date;

public class NodeSignature 
{
	Node node;
	long mtime = 0;
	
	public NodeSignature(Node n) 
	{
		node = n;
		mtime = node.file.lastModified();
	}
	
	long getTimeStamp()
	{
		return mtime;
	}
	
	void updateTimeStamp()
	{
		mtime = node.file.lastModified();
	}
	
	String getTimeStampAsString()
	{
		long d = getTimeStamp();
		if (d > 0)
			return new Date(getTimeStamp()).toString();
		else
			return "?";
	}
}
