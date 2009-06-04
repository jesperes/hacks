package se.eskilson.projecteuler;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.junit.Assert;
import org.junit.Test;

public class Problem54 {

	static enum Suite {
		Hearts,
		Clubs,
		Diamonds,
		Spades;
	}

	static enum Value implements Comparable<Value> {
		Two,
		Three,
		Four,
		Five,
		Six,
		Seven,
		Eight,
		Nine,
		Ten,
		Jack,
		Queen,
		King,
		Ace;
	}

	static Map<Character, Suite> suiteMap = new HashMap<Character, Suite>();
	static Map<Character, Value> valueMap = new HashMap<Character, Value>();

	static {
		suiteMap.put('H', Suite.Hearts);
		suiteMap.put('C', Suite.Clubs);
		suiteMap.put('D', Suite.Diamonds);
		suiteMap.put('S', Suite.Spades);

		valueMap.put('2', Value.Two);
		valueMap.put('3', Value.Three);
		valueMap.put('4', Value.Four);
		valueMap.put('5', Value.Five);
		valueMap.put('6', Value.Six);
		valueMap.put('7', Value.Seven);
		valueMap.put('8', Value.Eight);
		valueMap.put('9', Value.Nine);
		valueMap.put('T', Value.Ten);
		valueMap.put('J', Value.Jack);
		valueMap.put('Q', Value.Queen);
		valueMap.put('K', Value.King);
		valueMap.put('A', Value.Ace);

		System.out.println(Suite.Hearts.compareTo(Suite.Spades));
	}

	static class Hand {
		private final Set<Card> cards = new TreeSet<Card>();

		Hand(Card... cards) {
			for (Card c : cards) {
				this.cards.add(c);
			}
		}

		Hand(String... cards) {
			for (String c : cards) {
				Value value = valueMap.get(c.charAt(0));
				Suite suite = suiteMap.get(c.charAt(1));
				this.cards.add(new Card(suite, value));
			}
		}

		Hand(String cards) {
			this(cards.split(" "));
		}

		public boolean containsAllOf(Value... values) {
			for (Value v : values) {
				boolean found = false;
				for (Card c : cards) {
					if (c.value == v) {
						found = true;
						break;
					}
				}

				if (!found)
					return false;
			}

			return false;
		}

		public Set<Card> getCards() {
			return cards;
		}

		public boolean isRank(Rank rank) {
			return rank.check(this);
		}
	}

	static class Card implements Comparable<Card> {
		Suite suite;
		Value value;

		public Card(Suite suite, Value value) {
			if (suite == null || value == null)
				throw new IllegalArgumentException();

			this.suite = suite;
			this.value = value;
		}

		/**
		 * Create a card from a two-character string as given in the input file.
		 * 
		 * @param s
		 */
		public Card(String s) {
			suite = suiteMap.get(s.charAt(1));
			value = valueMap.get(s.charAt(0));
		}

		@Override
		public String toString() {
			return String.format("%s of %s", value, suite);
		}

		@Override
		public int compareTo(Card other) {
			return value.compareTo(other.value);
		}
	}

	enum Rank {

		RoyalFlush {
			@Override
			boolean check(Hand hand) {
				if (!Flush.check(hand))
					return false;

				return true;
			}
		},
		Flush {
			@Override
			boolean check(Hand hand) {
				Suite suite = null;
				for (Card c : hand.getCards()) {
					if (suite == null) {
						suite = c.suite;
					} else {
						if (suite != c.suite) {
							return false;
						}
					}
				}
				return true;
			}
		};

		/*
		 * Check if the given hand is of a given rank.
		 */
		abstract boolean check(Hand hand);
	}

	public static void main(String[] args) throws IOException {

		if (false) {
			File f = new File("poker.txt");
			BufferedReader reader = new BufferedReader(new InputStreamReader(
					new FileInputStream(f)));
			String s;
			while ((s = reader.readLine()) != null) {
				for (String t : s.split(" ")) {
					Card c = new Card(t);
					System.out.print(c + " ");
				}
				System.out.println();
			}
		}
	}

	@Test
	public void testCheckHand() {
		Hand hand;

		hand = new Hand("4D 3D 2D QD KD");
		Assert.assertTrue(hand.isRank(Rank.Flush));

		hand = new Hand("4D 3D 2S QD KD");
		Assert.assertFalse(hand.isRank(Rank.Flush));

		hand = new Hand("4D 3D 2D QD KD");
		Assert.assertFalse(hand.isRank(Rank.RoyalFlush));

		hand = new Hand("TD JD QD KD AD");
		Assert.assertTrue(hand.isRank(Rank.RoyalFlush));
	}
}
