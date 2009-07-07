package se.eskilson.toycompiler.ir;

public class IdentifierExpression implements IExpression {
	private final IIdentifier id;

	public IdentifierExpression(IIdentifier id) {
		this.id = id;
	}

	@Override
	public String toString() {
		return id.toString();
	}

	@Override
	public IExpression evaluate() {
		return this;
	}

	@Override
	public int intValue() throws InvalidTypeException {
		throw new InvalidTypeException("Not an integer", this);
	}

	@Override
	public double doubleValue() throws InvalidTypeException {
		throw new InvalidTypeException("Not a double", this);
	}

	@Override
	public long longValue() throws InvalidTypeException {
		throw new InvalidTypeException("Not a long", this);
	}
}
