# LZ77 compression encoder and decoder for deflate
This is a Lempel-Ziv77 [1] compression encoder/decoder for use in a deflate
encoder/decoder [2]. LZ77 is also called LZ1 [3].

## Exported functions

## Usage

## Encoder implementation
We implemented the single-level hashing attributed to gzip in [4]. The algorithm
is described in full in [4]. According to [4], we do not need a more elaborate
algorithm when using DEFLATE's encoding parameters. More involved algorithms
become necessary due to performance concerns only when using larger encoding
sliding windows.

We give a brief description of the encoding algorithm.

### Encoding algorithm
Let:
* `X = x[1..N] = x1 ... xN` the whole string to be compressed. Every `xi` is a
  character into the alphabet Σ of symbols to encode, fixnums in our case.
* `Sp = x[p..N]` The p-th suffix of X, *ie.* a subsequence of X from position p
  to its end.
* `Dp = x[p - DSIZ..p-1]` the sliding window (dictionary) when `x[1..p-1]` has
  already been encoded. This is the window in which we look for longest
  matching strings of symbols.
* `DSIZ` is the size of the sliding window.
* `DMASK = DSIZ - 1`
* `f_hash` a hashing function of a string of `M1` symbols.
* `M1` the smallest size for encoded strings. Matching strings below that size
  will be encoded as literals.
* `M2` the largest size for encoded strings. This is the upper bound for the
  `length` in the encoded length-distance pairs. Matching strings
  longer than `M2` will be encoded in multiple strings of length equal or below
  `M2`.

As per [1], the initial sliding window elements are all 0.

Taking the encoding at an arbitrary point, the whole message to be encoded is:
`X = Dp + Sp` where `+` is concatenation. `Dp` is the trail of symbols that
have already been encoded into length-distance pairs or literals and now
constitute the sliding window. `Sp` is the part of the message still left to
encode.

The general idea of the encoding algorithm, is that in order to encode a string
of symbols beginning at symbol `xp` and followed by `xp+1`, `xp+2` etc., we
need to look for the longest matching string in the sliding window. In other
words, we need to find the index `p'` for the beginning of a string whose
symbols will match those in the string beginning at index `p`, `xp = xp'`,
`xp+1 = xp'+1`, ... `xp+L-1 = xp'+L-1` with `L` the length of the longest
match.

A naive method would be to search linearly in the sliding window for the symbol
`xp` and then match one by one the following symbols in both strings while
keeping track of the longest length among matching strings. This is considered
too slow for practical purposes.

In lieu of this naive method, we save a hash table giving the index `p_match`
of a string in `X` given a hash of its first `M1` symbols. In this way, we have
much fewer strings to check than if we had to check for every string beginning
with symbol `xp` in the sliding window.

In order to execute this method, we need two tables:
* **head**: Keeps track of the index for the last string evaluating to a given 
  hash.
* **prev**: Keeps track of all the previous indices of strings that were
  hashed.  We can obtain all the strings evaluating to a given hash from this
  table (see below).

The steps to be performed when encoding the string beginning at symbol `xp`
are:
1. Compute the hash `h1 = f_hash(xp, xp+1, xp+M1-1)`.
2. Obtain the last matching string index `p_match` at `head(h1) = p_match`.
3. Check symbol by symbol the two strings at the current encoding position `p`
   and at `p_match`. `xp = xp_match`, `xp+1 = xp_match+1` etc. Save the length
   of the match `L`.
4. Look for the next match index `p_match2` at `prev(p_match & DMASK) =
   p_match2` Check the strings and keep track of longest match length `L`.
5. Go to the next match at `prev(p_match2 & DMASK)`, check the strings, update
   `L`.
6. Repeat step 5 until `p_match` goes out of the sliding window, there are no
   more matches. The length-distance pair is deduced from `L` and `p_match` for
   the longest match.
7. Now we update our tables, we insert the hash for the string beginning with
   `xp` of length `M1`.  `prev(p & DMASK) = head(h1)`, `head(h1) = p`. We do
   the same for all the symbols that were encoded along with `xp` in the
   length-distance pair `p+1`, `p+2` until `p+L-1`, updating the hash each time.
8. We are ready to repeat the steps for symbol `xp+L`.

When no match of length `M1` or more is found, `xp` is encoded as a literal,
its hash `h1` is inserted in the tables and we go to `xp+1`.  When two longest
matching strings are found, we choose the match with index closest to `p`.

## Caveats
* Only the default parameters for the encoder were tested, those used in
  DEFLATE [2].
* This library is not for compressing files per se. It outputs an intermediate
  representation that can be used for compression. See [2] to see an example of
  what can be done with this intermediate data.

## Test
To launch tests:

```common-lisp
(asdf:test-system "lz77")
```

## Performance
I did not yet look at performance issues. The encoding is algorithmically sound
for the DEFLATE parameters according to the benchmarks in [4].

## Dependencies
* `lz77`: None.
* `lz77/test`:
  * [rove](https://github.com/fukamachi/rove)

## Licensing
See the license file. Citations are appreciated but not needed.

## References
1. Ziv, Jacob, and Abraham Lempel. "A universal algorithm for sequential data compression." IEEE Transactions on information theory 23.3 (1977): 337-343 https://doi.org/10.1109/TIT.1977.1055714
2. https://www.w3.org/Graphics/PNG/RFC-1951
3. https://en.wikipedia.org/wiki/LZ77\_and\_LZ78
4. Sadakane, Kunihiko, and Hiroshi Imai. "Improving the speed of LZ77 compression by hashing and suffix sorting." IEICE transactions on fundamentals of electronics, communications and computer sciences 83.12 (2000): 2689-2698 https://www.researchgate.net/publication/238812180_Improving_the_Speed_of_LZ77_Compression_by_Hashing_and_Suffix_Sorting
