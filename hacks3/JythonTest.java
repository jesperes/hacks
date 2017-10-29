import org.python.util.PythonInterpreter;
import org.python.core.*;

public class JythonTest {
        public static void main(String[] argv) {
                PythonInterpreter python;
                PyObject append;

                python = new PythonInterpreter();
                python.exec("import sys");
                append = python.eval("sys.path.append(\"/home/jojo\")");
                python.exec("print sys.path");

        }
}
