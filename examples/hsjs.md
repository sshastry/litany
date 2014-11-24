How would we write this

```haskell
{- haskell/a.hs -}
sum = foldl (+) [1,2,3]
```

in JavaScript with the help of underscore? Here's how:

```javascript
// javascript/a.js
var sum = _.reduce([1, 2, 3], function(memo, num){ return memo + num; }, 0);
```

How about this?

```haskell
{- haskell/a.hs -}
evens = filter even [1..6]
```

Easy:

```javascript
// javascript/a.js
var evens = _.filter([1, 2, 3, 4, 5, 6], function(num){ return num % 2 == 0; });
```

Finally what about a lazy infinite stream of Fibonacci numbers?

```haskell
import Data.List (unfoldr)
fibs = unfoldr (\(a,b) -> Just (a,(b,a+b))) (0,1)
```

Not sure how to do that in js. Maybe ghcjs knows...
