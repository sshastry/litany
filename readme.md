
# Litany

Literate programming in any language.

A .lit file is a literate programming source file and also an archive file. Let's explain by an example, which can be found in `examples/hsjs.lit`.

```text
How would we write this

[[begin haskell haskell/a.hs]]
sum = foldl (+) [1,2,3]
[[pause haskell haskell/a.hs]]

in JavaScript with the help of underscore? Here's how:

[[begin javascript javascript/a.js]]
var sum = _.reduce([1, 2, 3], function(memo, num){ return memo + num; }, 0);
[[pause javascript javascript/a.js]]

How about this?

[[continue haskell haskell/a.hs]]
evens = filter even [1..6]
[[end haskell haskell/a.hs]]

Easy:

[[continue javascript javascript/a.js]]
var evens = _.filter([1, 2, 3, 4, 5, 6], function(num){ return num % 2 == 0; });
[[end javascript javascript/a.js]]

Finally what about a lazy infinite stream of Fibonacci numbers?

[[begin haskell]]
import Data.List (unfoldr)
fibs = unfoldr (\(a,b) -> Just (a,(b,a+b))) (0,1)
[[end haskell]]

Not sure how to do that in js. Maybe ghcjs knows...
```
Then
```
$ litany -m hsjs.lit
✓ hsjs.md
```
extracts a markdown file with the code blocks being bounded by triple backticks, and the beginning of a code block indicating the language.

This shows how the .lit file serves as an archive:
```
$ litany -e hsjs.lit
✓    2 "haskell/a.hs"
✓    2 "javascript/a.js"
```

The `-e` option extracts the files embedded in the blocks. Note that we can pause and continue the blocks, thus intermingling the code from different files in our literate file, but those files will be extracted to the right place by `-e`. Use both options `-m` and `-e` to extract from the archive and produce the markdown file simultaneously.

Finally, we can include a code block in the .lit file which is not intended to be extracted with `-e`, but which will appear in the markdown output. Simply omit a filename from such a block, as shown in the last block in hsjs.lit.

I found litany particularly useful when working on web apps, what with the constant switching and interweaved logic between model, view, and template files. In fact, work on a web app is what inspired litany in the first place.

# Installation
* `$ ghc -o litany Main.hs`

