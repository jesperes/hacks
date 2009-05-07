package se.eskilson.toycompiler;

import se.eskilson.toycompiler.ir.AssignmentStatement;
import se.eskilson.toycompiler.ir.BinaryOperator;
import se.eskilson.toycompiler.ir.BinaryOperatorExpression;
import se.eskilson.toycompiler.ir.CompoundStatement;
import se.eskilson.toycompiler.ir.IExpression;
import se.eskilson.toycompiler.ir.IStatement;
import se.eskilson.toycompiler.ir.Identifier;
import se.eskilson.toycompiler.ir.IdentifierExpression;
import se.eskilson.toycompiler.ir.IntegerExpression;
import se.eskilson.toycompiler.ir.InvalidTypeException;

public class ToyCompiler {

	/*
	 * 
	 * @param args
	 */
	public static void main(String[] args) throws InvalidTypeException {
		IStatement stm = new AssignmentStatement(new IdentifierExpression(
				new Identifier("a")), new IntegerExpression(42));

		IStatement cmpstm = new CompoundStatement(stm, stm);

		IExpression expr = new BinaryOperatorExpression(new IntegerExpression(
				42), BinaryOperator.Plus, new BinaryOperatorExpression(
				new IntegerExpression(15), BinaryOperator.Times,
				new IntegerExpression(47)));

		System.out.println(expr);
		System.out.println(expr.evaluate().intValue());
		System.out.println(expr.evaluate().doubleValue());
		System.out.println(expr.evaluate().longValue());

		System.out.println(new IdentifierExpression(new Identifier("a"))
				.evaluate().intValue());
	}
}
