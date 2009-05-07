package se.eskilson.toycompiler.ir;

public class BinaryOperatorExpression implements IExpression {

	private final IExpression left;
	private final BinaryOperator binOp;
	private final IExpression right;

	public BinaryOperatorExpression(IExpression left, BinaryOperator binOp,
			IExpression right) {
		this.left = left;
		this.binOp = binOp;
		this.right = right;
	}

	@Override
	public String toString() {
		switch (binOp) {
		case Plus:
			return String.format("(%s + %s)", left, right);
		case Times:
			return String.format("(%s * %s)", left, right);
		case Div:
			return String.format("(%s / %s)", left, right);
		case Minus:
			return String.format("(%s - %s)", left, right);
		}

		throw new AssertionError();
	}

	@Override
	public IExpression evaluate() throws InvalidTypeException {
		switch (binOp) {
		case Plus:
			return new IntegerExpression(left.evaluate().intValue()
					+ right.evaluate().intValue());
		case Minus:
			return new IntegerExpression(left.evaluate().intValue()
					- right.evaluate().intValue());
		case Times:
			return new IntegerExpression(left.evaluate().intValue()
					* right.evaluate().intValue());
		case Div:
			return new IntegerExpression(left.evaluate().intValue()
					/ right.evaluate().intValue());
		}

		throw new AssertionError();
	}

	@Override
	public int intValue() throws InvalidTypeException {
		return evaluate().intValue();
	}

	@Override
	public double doubleValue() throws InvalidTypeException {
		return evaluate().doubleValue();
	}

	@Override
	public long longValue() throws InvalidTypeException {
		return evaluate().longValue();
	}
}
