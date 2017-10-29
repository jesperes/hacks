package bamboo.engine;

public class Bamboo 
{
	public static void main(String[] args) 
	{
		long t0 = System.currentTimeMillis();

		Node target = new Node("testsrc/nodes/root");

		target.populateDependencies(5, 5, "testsrc/nodes/node");

		Scheduler b = new Scheduler();
		boolean result = b.build(target);
		
		t0 = System.currentTimeMillis() - t0;
		
		System.out.println("Result: " + result + " (" + t0 + " msecs)");
	}
}
 