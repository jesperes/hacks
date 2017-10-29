class PythonOutputThread extends Thread {
   
    Process proc;
    
    public PythonOutputThread(Process pythonproc) {
        proc = pythonproc;
        setDaemon(true);
    }
    
    public void run() {
        
        while (true) {
            try {
                proc.exitValue();
                System.out.println("Python process exited.");
                break;
            } catch (IllegalThreadStateException e) {
            }
            
            try {            
                int c = proc.getInputStream().read();

                System.out.println(Integer.toString(c));
            } catch (Exception e) {
                System.out.println("IOException: " + e);
            }
        }
    }
}

public class Foo {
        public static void main(String argv[]) {
    	PythonOutputThread pythr;
    	String args = "python -i -c 'print \"this is python\"'";
    	
    	try {
    	    Process proc = Runtime.getRuntime().exec(args);
        	pythr = new PythonOutputThread(proc);
        	pythr.start();
    	} catch (IOException e) {
    	    System.out.println(e);
    	}
}
