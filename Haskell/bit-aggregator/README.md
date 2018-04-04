# bit-aggregator

This program is inspired by a little problem I was facing. The actual problem
was harder, but I choose to implement part of it in Haskell.

## The task

Write a program that receives the path to a CSV (comma separated value) encoded
file, separated by commas, and yield bytes.

The file holds the following content: An index column of timestamps and at
least one more column of some "category", which represents raw bit (not byte)
values. The task is to parse the file row by row and to aggregate (hence the
name of the program) to bytes.

The timestamps are not serving any real purpose in this example, but are an
artifact of the original problem und useful to keep. The timestamp value of the
first bit of a byte can be seen as the "emergence time" of the byte.

For each category column return a vector/array/list of the bytes it constitutes
together with its "emergence time".
