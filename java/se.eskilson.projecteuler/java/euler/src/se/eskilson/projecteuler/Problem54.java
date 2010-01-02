package se.eskilson.projecteuler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
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

	static EnumSet<Value> allValues = EnumSet.allOf(Value.class);
	static EnumSet<Suite> allSuites = EnumSet.allOf(Suite.class);
	static EnumSet<Rank> allRanks = EnumSet.allOf(Rank.class);
	static List<Rank> sortedRanks = new ArrayList<Rank>(allRanks);

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

		Collections.sort(sortedRanks);
		Collections.reverse(sortedRanks);
	}

	static class Hand implements Comparable<Hand> {
		private final Set<Card> cards = new TreeSet<Card>();

		// The list of card values is sorted in high-to-low order.
		private final List<Value> values = new ArrayList<Value>();

		// The values composing the rank
		public List<Value> rankValues;

		// Other values.
		public List<Value> nonRankValues;

		private Rank rank = null;

		private final void init() {
			if (this.cards.size() != values.size()) {
				throw new AssertionError("duplicate cards");
			}

			Collections.sort(this.values);
			Collections.reverse(this.values);

			for (Rank r : sortedRanks) {
				if (r.check(this)) {
					rank = r;
				}
			}

			if (rank == null)
				throw new AssertionError();
		}

		Hand(Card... cards) {
			for (Card c : cards) {
				this.cards.add(c);
				this.values.add(c.value);
			}

			init();
		}

		Hand(String... cards) {
			for (String c : cards) {
				Value value = valueMap.get(c.charAt(0));
				Suite suite = suiteMap.get(c.charAt(1));
				this.cards.add(new Card(suite, value));
				this.values.add(value);
			}

			init();
		}

		Hand(String cards) {
			this(cards.split(" "));
		}

		public Set<Card> getCards() {
			return cards;
		}

		public boolean isRank(Rank rank) {
			return this.rank == rank;
		}

		/**
		 * List of values in the hand, sorted in high-to-low order.
		 * 
		 * @return
		 */
		public List<Value> getValues() {
			return values;
		}

		/**
		 * Return the rank of this hand (the highest applicable rank).
		 * 
		 * @return
		 */
		public Rank getRank() {
			return rank;
		}

		/**
		 * Checks if the hand contains exactly n cards of the given value.
		 * 
		 * @param value
		 * @param n
		 * @return
		 */
		public boolean hasNumberOf(Value value, int n) {
			int found = 0;
			for (Value v : values) {
				if (v == value)
					found++;
			}

			return (found == n);
		}

		@Override
		public String toString() {
			return cards.toString();
		}

		/**
		 * Two hands are equals if the set of cards is equal.
		 */
		@Override
		public boolean equals(Object obj) {
			if (!(obj instanceof Hand))
				return false;

			Hand other = (Hand) obj;
			return cards.equals(other.cards);
		}

		@Override
		public int hashCode() {
			return cards.hashCode();
		}

		/**
		 * Comparing two hands relies on the Rank enum ordinal to correspond to
		 * the value of the Rank.
		 */
		@Override
		public int compareTo(Hand other) {
			if (equals(other))
				return 0;

			Rank otherRank = other.getRank();
			Rank thisRank = getRank();
			if (otherRank != thisRank) {
				return thisRank.compareTo(otherRank);
			} else {
				for (int i = 0; i < values.size(); i++) {
					Value thisValue = values.get(i);
					Value otherValue = other.getValues().get(i);
					int compare = thisValue.compareTo(otherValue);
					if (compare == 0)
						continue;
					else
						return compare;
				}

				throw new AssertionError(String.format(
						"equals and compareTo mismatch: %s, %s", this, other));
			}
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
			if (other.suite != suite) {
				return suite.compareTo(other.suite);
			} else {
				return value.compareTo(other.value);
			}
		}

		@Override
		public boolean equals(Object other) {
			if (other instanceof Card) {
				Card otherCard = (Card) other;
				return compareTo(otherCard) == 0;
			} else {
				return false;
			}
		}

		@Override
		public int hashCode() {
			return suite.hashCode() ^ value.hashCode();
		}
	}

	/**
	 * Lowest ranked hand first.
	 * 
	 */
	enum Rank {
		HighCard {
			@Override
			boolean check(Hand hand) {
				hand.nonRankValues.addAll(hand.values);
				return true;
			}
		},
		OnePair {
			@Override
			boolean check(Hand hand) {
				for (Value val : allValues) {
					if (hand.hasNumberOf(val, 2)) {
						hand.rankValues.add(val);
						return true;
					} else {
						hand.nonRankValues.add(val);
					}
				}

				return false;
			}
		},
		TwoPair {
			// CONTINUE HERE with setting rankValues and nonRankValues properly.
			@Override
			boolean check(Hand hand) {
				for (Value val1 : allValues) {
					if (hand.hasNumberOf(val1, 2)) {
						for (Value val2 : allValues) {
							if (hand.hasNumberOf(val2, 2) && val1 != val2) {
								return true;
							}
						}
					}
				}
				return false;
			}
		},
		ThreeOfAKind {
			@Override
			boolean check(Hand hand) {
				for (Value val : allValues) {
					if (hand.hasNumberOf(val, 3))
						return true;
				}

				return false;
			}

		},
		Straight {
			@Override
			boolean check(Hand hand) {
				LinkedList<Value> list = new LinkedList<Value>(hand.getValues());
				Collections.sort(list);
				Value first = list.getFirst();
				Value last = list.getLast();
				return (last.ordinal() - first.ordinal() + 1) == list.size();
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
		},
		FullHouse {
			@Override
			boolean check(Hand hand) {
				return ThreeOfAKind.check(hand) && OnePair.check(hand);
			}

		},
		FourOfAKind {
			@Override
			boolean check(Hand hand) {
				for (Value value : allValues) {
					if (hand.hasNumberOf(value, 4))
						return true;
				}
				return false;
			}
		},
		StraightFlush {
			@Override
			boolean check(Hand hand) {
				return Flush.check(hand) && Straight.check(hand);
			}
		},
		RoyalFlush {
			@Override
			boolean check(Hand hand) {
				if (!Flush.check(hand))
					return false;

				Collection<Value> values = hand.getValues();
				if (values.contains(Value.Ten) && values.contains(Value.Jack)
						&& values.contains(Value.Queen)
						&& values.contains(Value.King)
						&& values.contains(Value.Ace)) {
					return true;
				} else {
					return false;
				}
			}
		};

		/**
		 * Check if the given hand is of a given rank.
		 * 
		 * @param hand
		 *            The hand to check.
		 * @param list
		 *            The check method will add values to this list to be used
		 *            as tie-breakers for hands with equal ranks.
		 * 
		 */
		abstract boolean check(Hand hand);

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

		hand = new Hand("2D 3D 4D 5D 6D");
		Assert.assertTrue(hand.isRank(Rank.StraightFlush));
		hand = new Hand("2D 3D 4D 5D 7D");
		Assert.assertFalse(hand.isRank(Rank.StraightFlush));

		hand = new Hand("2D 3S 4C 5H 6D");
		Assert.assertTrue(hand.isRank(Rank.Straight));
		hand = new Hand("2D 3D 4D 5D 7D");
		Assert.assertFalse(hand.isRank(Rank.Straight));

		hand = new Hand("2D 2S 2H 2C 6D");
		Assert.assertTrue(hand.isRank(Rank.FourOfAKind));
		hand = new Hand("2D 3D 4D 5D 7D");
		Assert.assertFalse(hand.isRank(Rank.FourOfAKind));

		hand = new Hand("2D 2S 2H 6C 6D");
		Assert.assertTrue(hand.isRank(Rank.FullHouse));
		hand = new Hand("2D 3D 4D 5D 7D");
		Assert.assertFalse(hand.isRank(Rank.FullHouse));

		hand = new Hand("2D 2S 2H 6C 8D");
		Assert.assertTrue(hand.isRank(Rank.ThreeOfAKind));
		hand = new Hand("2D 3D 4D 5D 7D");
		Assert.assertFalse(hand.isRank(Rank.ThreeOfAKind));

		hand = new Hand("2D 2S 3H 4C 5D");
		Assert.assertTrue(hand.isRank(Rank.OnePair));
		hand = new Hand("2D 3D 4D 5D 7D");
		Assert.assertFalse(hand.isRank(Rank.OnePair));

		hand = new Hand("2D 2S 3H 3C 5D");
		Assert.assertTrue(hand.isRank(Rank.TwoPair));

		Hand flush = new Hand("2D 3D 4D 5D 7D");
		Hand straightFlush = new Hand("2D 3D 4D 5D 6D");
		Hand royalFlush = new Hand("AD KD QD JD TD");
		Hand straight = new Hand("2D 3D 4S 5C 6H");
		Hand fullHouse = new Hand("2D 2H 4S 4C 4H");
		Hand fourOfAKind = new Hand("2D 2H 2C 2S 4S");
		Hand threeOfAKind = new Hand("2D 2H 2C 3H 4S");
		Hand twoPair = new Hand("2D 2H 3C 3H 4S");
		Hand onePair = new Hand("2D 2H 3C 4H 5S");
		Hand highCard = new Hand("2D 3H 4C 5H 7S");

		Assert.assertEquals(Rank.Flush, flush.getRank());
		Assert.assertEquals(Rank.StraightFlush, straightFlush.getRank());
		Assert.assertEquals(Rank.RoyalFlush, royalFlush.getRank());
		Assert.assertEquals(Rank.Straight, straight.getRank());
		Assert.assertEquals(Rank.FullHouse, fullHouse.getRank());
		Assert.assertEquals(Rank.FourOfAKind, fourOfAKind.getRank());
		Assert.assertEquals(Rank.ThreeOfAKind, threeOfAKind.getRank());
		Assert.assertEquals(Rank.TwoPair, twoPair.getRank());
		Assert.assertEquals(Rank.OnePair, onePair.getRank());
		Assert.assertEquals(Rank.HighCard, highCard.getRank());

		Assert.assertEquals(5, flush.compareTo(highCard));
		Assert.assertEquals(4, flush.compareTo(onePair));
		Assert.assertEquals(3, flush.compareTo(twoPair));
		Assert.assertEquals(2, flush.compareTo(threeOfAKind));
		Assert.assertEquals(1, flush.compareTo(straight));
		Assert.assertEquals(0, flush.compareTo(flush));
		Assert.assertEquals(-1, flush.compareTo(fullHouse));
		Assert.assertEquals(-2, flush.compareTo(fourOfAKind));
		Assert.assertEquals(-3, flush.compareTo(straightFlush));
		Assert.assertEquals(-4, flush.compareTo(royalFlush));

		// Reference set of hands from the problem description
		Hand h1p1 = new Hand("5H 5C 6S 7S KD");
		Assert.assertEquals(Rank.OnePair, h1p1.getRank());

		Hand h1p2 = new Hand("2C 3S 8S 8D TD");
		Assert.assertEquals(Rank.OnePair, h1p2.getRank());

		Assert.assertEquals(-3, h1p1.compareTo(h1p2));
	}
}
