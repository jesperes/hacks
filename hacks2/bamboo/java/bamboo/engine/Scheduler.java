package bamboo.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

public class Scheduler {
	
	final static int numThreads = 16;
	
	Collection<Future<Boolean>> results;
	ExecutorService threadPool;
	
	int numJobs;
	
	boolean build(Node root)
	{
		boolean result = false;
		int numNodes;
		
		numNodes = root.updateDirtyMark();

		threadPool = Executors.newFixedThreadPool(numThreads);
		results = new ArrayList<Future<Boolean>>();
		
		numJobs = 0;
		
		BuildJob rootJob = createJobs(root);

		System.out.println("Number of nodes checked: " + numNodes);
		System.out.println("Number of jobs to run: " + numJobs);
		
		if (rootJob != null)
		{
			try {
				result = rootJob.result.get();
				
				if (!result)
					System.out.println("Root build job failed.");
				
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ExecutionException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		else
		{
			System.out.println("Nothing to be done for: " + root);
			result = true;
		}
		
		threadPool.shutdown();
		try {
			threadPool.awaitTermination(10, TimeUnit.SECONDS);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		
		return result;
	}
	

	/**
	 * Create BuildJob objects for root and all its children (if they are dirty),
	 * and submit them to the threadpool for execution.
	 * 
	 * @param root
	 * @return
	 */
	BuildJob createJobs(Node root)
	{
		if (root.dirty)
		{
			Collection<BuildJob> depJobs = null;

			for (Node dep: root.deps)
			{
				BuildJob depJob = createJobs(dep);
				
				if (depJob != null)
				{
					if (depJobs == null)
						depJobs = new ArrayList<BuildJob>();
					
					depJobs.add(depJob);
				}
			}

			BuildJob job = new BuildJob(root);
			numJobs++;
			
			job.dependencies = depJobs;

			// Submit the job to the thread pool for execution.
			job.result = threadPool.submit(job);

			return job;
			
		} else {
			return null;
		}
	}
}
