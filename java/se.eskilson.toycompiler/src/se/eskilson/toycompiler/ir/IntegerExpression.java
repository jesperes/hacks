package se.eskilson.toycompiler.ir;

public class IntegerExpression implements IExpression {

	private final int num;

	public IntegerExpression(int num) {
		this.num = num;
	}

	@Override
	public String toString() {
		return String.format("%d", num);
	}

	@Override
	public IExpression evaluate() {
		return this;
	}

	@Override
	public int intValue() throws InvalidTypeException {
		return num;
	}

	@Override
	public double doubleValue() throws InvalidTypeException {
		return num;
	}

	@Override
	public long longValue() throws InvalidTypeException {
		return num;
	}
}
