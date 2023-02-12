---
title: "Comparison of a cute algorithm in JavaScript and Haskell"
date: 2023-02-12
published: true
---

```haskell

import Control.Monad.Random
import Data.Set (Set)
import qualified Data.Set as Set

countDistinct :: (MonadRandom m, Ord a) => Int -> [a] -> m Double
countDistinct threshold xs = do
  (acc, p) <- foldM go (Set.empty, 1) xs
  return $ fromIntegral (Set.size acc) / p
  where
    go (acc, p) x = do
      -- Add the element if lucky, ignoring the existing value
      acc' <- Set.alterF (\_ -> isLucky p) x acc

      -- If the set reached the threshold, make it twice as hard
      if Set.size acc' < threshold
        then return (acc', p)
        else (,) <$> thanos acc' <*> return (p / 2)

-- Remove half of the elements in a set, randomly
thanos :: (MonadRandom m, Eq a) => Set a -> m (Set a)
thanos =
  fmap Set.fromAscList
    . filterM (\_ -> isLucky 0.5)
    . Set.toAscList

-- Toss a coin with given probability of 'True'
isLucky :: MonadRandom m => Double -> m Bool
isLucky p = (< p) <$> getRandom
```

For comparison, here's the JS version on the post above:


```typescript
function countDistinct(list, thresh) {
    let p = 1;
    let seen = new Set();
    for (let value of list) {
        seen.delete(value);
        if (Math.random() < p) {
            seen.add(value);
        }
        if (seen.size === thresh) {
            // Objects now need to win an extra coin flip to be included
            // in the set. Every element in `seen` already won n-1 coin
            // flips, so they now have to win one more.
            seen = new Set([...seen].filter(() => Math.random() < 0.5));
            p *= 1 / 2;
        }
    }
    return seen.size / p;
}
```
