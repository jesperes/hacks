package se.eskilson.toycompiler.ir;

public class AssignmentStatement implements IStatement {

	private final IdentifierExpression id;
	private final IExpression expr;

	public AssignmentStatement(IdentifierExpression id, IExpression expr) {
		this.id = id;
		this.expr = expr;
	}

	@Override
	public String toString() {
		return String.format("%s = %s", id, expr);
	}
}
