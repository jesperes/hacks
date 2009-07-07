package se.eskilson.toycompiler.ir;

public class Identifier implements IIdentifier {
	private final String id;

	public Identifier(String id) {
		this.id = id;
	}

	@Override
	public String toString() {
		return id;
	}
}
