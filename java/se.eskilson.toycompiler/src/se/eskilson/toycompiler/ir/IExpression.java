package se.eskilson.toycompiler.ir;

public interface IExpression {
	public IExpression evaluate() throws InvalidTypeException;

	public int intValue() throws InvalidTypeException;

	public long longValue() throws InvalidTypeException;

	public double doubleValue() throws InvalidTypeException;
}
