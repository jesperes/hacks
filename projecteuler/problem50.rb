require 'sieve'


def verify_sum(sum, list)
  actual = list.inject(0) do |s,n| s + n end
  if sum != actual
    raise "expected sum = #{sum}, actual sum = #{actual}"
  end
end

def problem50(limit) 
  puts "Getting primes (#{limit})"
  primes = sieve(limit)
  prime_hash = primes.inject({}) do |hash, p| hash[p] = true ; hash end
  puts "There are #{primes.length} primes under #{limit}"

  largest_prime = primes.last
  puts "Largest prime: #{largest_prime}"
  
  sum_primes = 0

  # For each prime p, value == the sum of all primes up to and
  # including p.
  all_prime_sums = {}

  # If there exists a sequence P of length N, the sum of the N first
  # primes must be less than the largest prime, since all other
  # sequences of N consecutive primes are larger.
  max_sequence_length = 0
  primes.each do |p|
    sum_primes += p
    all_prime_sums[p] = sum_primes
    if sum_primes < largest_prime
      max_sequence_length += 1
    end
  end
  puts "Maximum sequence length: #{max_sequence_length}"
  puts "Sum of all primes: #{sum_primes}"
  
  sum = 0
  sublist = []
  
  max_sequence_length.downto(1) do |seqlen|
    num_sequences = (primes.length - seqlen + 1)
    num_sequences.times do |i|
      if i == 0
        if seqlen == primes.length
          # Case 1: checking the entire list of primes

          sublist = primes
          sum = sum_primes
          verify_sum(sum, sublist)
        else
          # Case 2: We're restart with a new sequence length.  This
          # case breaks the first time we run it after having running
          # case 3.

          sublist = primes.first(seqlen)
          sum = all_prime_sums[sublist.last]
          verify_sum(sum, sublist)
        end
      else
        # Case 3: check the next sequence. Subtract the first element
        # of the old sequence, and add the next element.

        first = sublist.shift
        pnext = primes[i + seqlen - 1]
        sublist = sublist << pnext
        sum -= first
        sum += pnext
        verify_sum(sum, sublist)
      end
      
      # Exit on first found sum which is a prime.
      if prime_hash.has_key?(sum)
        puts "Prime = #{sum}"
        puts "Sequence = #{sublist.inspect} (#{sublist.length} elements)"
        exit 0
      end
      
      # If the sum exceeds the largest prime, we can skip all following
      # sequences of this length (since they are growing monotonically).
      if sum > largest_prime
        # puts "Sum exceeds largest prime, skipping rest of #{seqlen}-sequences."
        break
      end
    end
  end
end

if __FILE__ == $0
  puts problem50(ARGV.shift.to_i)
end
