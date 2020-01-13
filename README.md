# LZ77 compression encoder and decoder for deflate
This is a Lempel-Ziv77 [1] compression encoder/decoder for use in a deflate
encoder/decoder [2]. LZ77 is also called LZ1 [3].

## Exported functions

## Usage

## Encoder implementation


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
