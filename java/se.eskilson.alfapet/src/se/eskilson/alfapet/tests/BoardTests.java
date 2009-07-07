package se.eskilson.alfapet.tests;

import org.junit.Before;
import org.junit.Test;

import se.eskilson.alfapet.Board;
import se.eskilson.alfapet.Board.Direction;

public class BoardTests {

	private Board board;

	@Before
	public void setUp() throws Exception {
		board = new Board();
	}

	@Test
	public void testPlaceWord() {
		board.placeWord("foobar", 0, 0, Direction.Across);
		System.out.println(board);
	}

}
