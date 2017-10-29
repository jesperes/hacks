package bamboo.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class BuildJob implements Callable<Boolean>
{
	Node node;
	Collection<BuildJob> dependencies = 
			new ArrayList<BuildJob>();
	Future<Boolean> result;
	
	static Random rand;

	static {
 		rand = new Random(System.currentTimeMillis());
	}
	
	BuildJob(Node node)
	{
		this.node = node;
	}

	void waitSome() throws InterruptedException
	{
		Thread.sleep(rand.nextInt(1000));
	}
	
	public Boolean call() throws InterruptedException, ExecutionException
	{
		// First, wait for all dependencies to generate their results.
		for (BuildJob dep: dependencies)
		{
			Boolean result = dep.result.get();
			
			if (!result)
			{
				System.err.println(this + ": dep job failed: " + dep);
				return false;
			}
		}

		try {
			
			if (node.file.exists())
				node.file.setLastModified(System.currentTimeMillis());
			else
				node.file.createNewFile();
			
			node.sig.updateTimeStamp();

			//waitSome();

			//System.out.println(Thread.currentThread() + 
			//		": successfully built node " + node);
			return true;
		} catch (Exception e) {
			System.err.println("Could not build: " + 
					node.file.getAbsolutePath());
			return false;
		}
	}
}
