package se.eskilson.toycompiler.ir;

public class CompoundStatement implements IStatement {
	private final IStatement stm1;
	private final IStatement stm2;

	public CompoundStatement(IStatement stm1, IStatement stm2) {
		this.stm1 = stm1;
		this.stm2 = stm2;
	}

	@Override
	public String toString() {
		return String.format("%s ; %s", stm1, stm2);
	}
}
