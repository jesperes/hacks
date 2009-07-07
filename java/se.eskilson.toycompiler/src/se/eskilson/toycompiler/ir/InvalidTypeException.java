package se.eskilson.toycompiler.ir;

public class InvalidTypeException extends Exception {
	private final IExpression expr;
	private final String msg;

	public InvalidTypeException(String msg, IExpression expr) {
		this.msg = msg;
		this.expr = expr;
	}

	private static final long serialVersionUID = 1L;

	@Override
	public String toString() {
		return String.format("%s: %s", msg, expr);
	}
}
