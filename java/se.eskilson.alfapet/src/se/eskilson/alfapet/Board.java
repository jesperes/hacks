package se.eskilson.alfapet;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Board {
	public static enum Direction {
		Across, Down
	}

	private static enum Multiplier {
		P0() {
			@Override
			int multiply(String word, int i) {
				return LetterPoints.valueOf(word.charAt(i));
			}
		},
		L2 {
			@Override
			int multiply(String word, int i) {
				return LetterPoints.valueOf(word.charAt(i)) * 2;
			}
		},
		L3 {
			@Override
			int multiply(String word, int i) {
				return LetterPoints.valueOf(word.charAt(i)) * 3;
			}
		},
		W2() {
			@Override
			int multiply(String word, int i) {
				int sum = 0;
				for (char c : word.toCharArray()) {
					sum += LetterPoints.valueOf(c);
				}
				return sum * 2;
			}
		},
		W3 {
			@Override
			int multiply(String word, int i) {
				int sum = 0;
				for (char c : word.toCharArray()) {
					sum += LetterPoints.valueOf(c);
				}
				return sum * 3;
			}
		};

		abstract int multiply(String word, int i);
	}

	private static class Square {
		String c;
		final String repr;
		final Multiplier multiplier;

		public void setChar(String c) {
			this.c = c;
		}

		@Override
		public String toString() {
			if (c == null) {
				return repr;
			} else {
				return new String(c);
			}
		}

		Square(String repr, Multiplier m) {
			this.repr = repr;
			multiplier = m;
		}

		Square(Square sq) {
			repr = sq.repr;
			multiplier = sq.multiplier;
		}
	}

	public final int BOARD_SIZE = 15;
	public final int BOARD_HALFL = 7;
	public final int BOARD_HALFH = 8;
	List<List<Square>> board = new ArrayList<List<Square>>();

	private void addRow(Square... sq) {
		List<Square> list = new ArrayList<Square>();
		list.addAll(Arrays.asList(sq));
		for (Square p : sq) {
			list.add(BOARD_HALFH, new Square(p));
		}
		list.remove(BOARD_HALFL);
		board.add(list);
	}

	private Square P0() {
		return new Square("__", Multiplier.P0);
	}

	private Square L2() {
		return new Square("2L", Multiplier.L2);
	}

	private Square L3() {
		return new Square("3L", Multiplier.L3);
	}

	private Square W2() {
		return new Square("2W", Multiplier.W2);
	}

	private Square W3() {
		return new Square("3W", Multiplier.W3);
	}

	public Board() {
		addRow(W3(), P0(), P0(), L2(), P0(), P0(), P0(), W3());
		addRow(P0(), P0(), W2(), P0(), P0(), P0(), L2(), P0());
		addRow(P0(), W2(), P0(), P0(), P0(), L3(), P0(), P0());
		addRow(L2(), P0(), P0(), P0(), W2(), P0(), P0(), P0());
		addRow(P0(), P0(), P0(), W2(), P0(), P0(), P0(), P0());
		addRow(P0(), P0(), L3(), P0(), P0(), P0(), L2(), P0());
		addRow(P0(), L2(), P0(), P0(), P0(), L2(), P0(), P0());
		addRow(W3(), P0(), P0(), P0(), P0(), P0(), P0(), W2());
		addRow(P0(), L2(), P0(), P0(), P0(), L2(), P0(), P0());
		addRow(P0(), P0(), L3(), P0(), P0(), P0(), L2(), P0());
		addRow(P0(), P0(), P0(), W2(), P0(), P0(), P0(), P0());
		addRow(L2(), P0(), P0(), P0(), W2(), P0(), P0(), P0());
		addRow(P0(), W2(), P0(), P0(), P0(), L3(), P0(), P0());
		addRow(P0(), P0(), W2(), P0(), P0(), P0(), L2(), P0());
		addRow(W3(), P0(), P0(), L2(), P0(), P0(), P0(), W3());
	}

	@Override
	public String toString() {
		StringBuilder buf = new StringBuilder();

		for (List<Square> row : board) {
			for (Square sq : row) {
				buf.append(String.format("%3s", sq.toString().toUpperCase()));
			}
			buf.append("\n");
		}

		return buf.toString();
	}

	public boolean placeWord(String word, int row, int col, Direction dir) {
		if (row < 0 || row >= BOARD_SIZE) {
			throw new IllegalArgumentException();
		}
		if (col < 0 || col >= BOARD_SIZE) {
			throw new IllegalArgumentException();
		}
		switch (dir) {
		case Across:
			if (col + word.length() > BOARD_SIZE) {
				System.out.println("Word is too long: " + word);
				return false;
			}

			List<Square> sublist = board.get(row).subList(col,
					col + word.length());
			for (int i = 0; i < word.length(); i++) {
				sublist.get(i).setChar(String.valueOf(word.charAt(i)));
			}
			return true;
		case Down:
			if (row + word.length() > BOARD_SIZE) {
				System.out.println("Word is too long: " + word);
				return false;
			}
			return true;
		}

		return false;
	}
}
