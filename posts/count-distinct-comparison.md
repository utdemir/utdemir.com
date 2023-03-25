---
title: "Short comparison of a neat Count-Distinct algorithm in JavaScript and Haskell"
date: 2023-02-22
published: true
---

Recently, I read a fun article called ["A Charming Algorithm for Count-Distinct"][original-post] from Justin Jaffray.
If you haven't read it already, I suggest you do as it's much more entertaining than this one 🙂. It explains a concise algorithm to estimate the number of distinct elements in a set in bounded memory.
Below is the JavaScript implementation from the article, which is very straightforward once you know the algorithm (see the linked article for a neat explanation):

[original-post]: https://justinjaffray.com/a-charming-algorithm-for-count-distinct/

<div style="width: fit-content; margin: 0 auto; font-size: 90%">

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

</div>

I indeed found the algorithm very charming.
As a Haskeller, seeing a neat algorithm forces me to try to write it in Haskell to see how it'd look like.
My initial instinct was that it would look cleaner and more concise.
However, once I wrote it, it did not feel "unequivocally better".
After staring at it for some time, I found parts that I appreciated in the Haskell implementation, and other parts that felt like noise.

Before I ramble on further, here is my Haskell implementation:

<div style="width: fit-content; margin: 0 auto; font-size: 90%">

```haskell
import Control.Monad.Random
import Data.Set (Set)
import qualified Data.Set as Set

countDistinct :: (MonadRandom m, Ord a) => Int -> [a] -> m Double
countDistinct threshold xs = do
  (seen, p) <- foldM go (Set.empty, 1) xs
  return $ fromIntegral (Set.size seen) / p
 where
  go (seen, p) value = do
    -- Add the element if lucky, ignoring the existing value
    seen' <- Set.alterF (\_ -> isLucky p) value seen

    -- If the set reached the threshold, make it twice as hard
    if Set.size seen' < threshold
      then return (seen', p)
      else (,) <$> thanos seen' <*> return (p / 2)

-- Remove half of the elements in a set, randomly
thanos :: MonadRandom m => Set a -> m (Set a)
thanos =
  fmap Set.fromDistinctAscList
    . filterM (\_ -> isLucky 0.5)
    . Set.toAscList

-- Toss a coin with given probability of 'True'
isLucky :: MonadRandom m => Double -> m Bool
isLucky p = (< p) <$> getRandom
```

</div>

Just stare at them for a bit. I'll wait.

. . .


They are so similar, yet so different, right? Even as a seasoned Haskell programmer, I find the Haskell implementation very wordy. There are types, constraints, monads, operators and so on.

This is just a fun exercise, so I try not to go over them in detail, but just share some of my thoughts.

First, some disclaimers:

* I know that there are a lot of ways to get the Haskell implementation to look more like the JavaScript one and vice versa. My objective is not to make the languages do weird things, but to try see how the choice of the programming language changes the way we express the same algorithm. So, I tried to write the Haskell implementation in a way that I'd write it in a real project.
* Similarly, I am depending on `containers` and `MonadRandom` Haskell packages, but the JavaScript code is standalone. I think this is still fair, because they are de-facto libraries in the Haskell ecosystem; but JavaScript has no such equivalent. In other words, I believe the average Haskell implementation of this algorithm is likely to use these libraries, but the average JavaScript implementation is likely to use the builtin `Set` and `Math.random()`.
* Obviously Haskell is typed and JavaScript is untyped. I try not to focus on types in this post; we could convert the JavaScript implementation to TypeScript with a couple of type annotations, but the guarantees provided by Haskell and TypeScript type system is very different so the comparison wouldn't mean much.

And now to the specific points.

### Use of MonadRandom

<div class="pure-g">
<div class="pure-u-1 pure-u-md-1-2"><div style="padding-right: 1px">

```javascript
Math.random() < p
```

</div></div>
<div class="pure-u-1 pure-u-md-1-2"><div style="padding-left: 1px">

```haskell
-- Toss a coin with given probability of 'True'
isLucky :: MonadRandom m => Double -> m Bool
isLucky p = (< p) <$> getRandom
```

</div></div>
</div>

I believe this is the biggest difference between the two implementations.
In most languages, no one would bat an eye for an expression like `Math.random() < p`.
However, as Haskell is a pure language, an expression can't just change the state of the world (in this case, state of the random number generator).
So, we use `MonadRandom` to carry around the state of the random number generator, but this forces us to lift our expressions into the monad often, often with weird-looking operators like `<$>` and `<*>`.

This use of monads does introduce some verbosity, but it also brings some benefits.
Most importantly, the random number generation is now explicit, and we can use it to test our code with a fixed seed to get deterministic output; and use a "real" random seed in production.

Again, surely there must be hundreds of libraries on `npm` that provides a deterministic random number generator in JavaScript, but I don't think people would reach out to them; and their implementation would either allow setting the seed globally which makes it less composable, or require passing the RNG (random number generator) around which would make the code more verbose.

In the end, I do think the "default" Haskell implementation has a lot of benefits over the "default" JavaScript implementation. However, those benefits do come with a syntactical overhead.

### Set interface

<div class="pure-g">
<div class="pure-u-1 pure-u-md-1-2"><div style="padding-right: 1px">

```javascript
seen.delete(value);
if (Math.random() < p) {
  seen.add(value);
}
```

</div></div>
<div class="pure-u-1 pure-u-md-1-2"><div style="padding-left: 1px">

```haskell
Set.alterF (\_ -> isLucky p) value seen
```

</div></div>
</div>

`alterF` is a function that can "modify" the presence of an element in the set while possibly having access to a context (the RNG in this case). Here, the function we pass to `alterF` ignores its parameter, making it obvious that the existence of the element in the set does not matter.

This seems like a minor point, but I think it's a good example of how the two languages differ in their approach to data structures. While the Set in the JS spec comes with [a couple of useful functions][javascript-set-methods], Haskell's `Data.Set` module comes with [a much more powerful interface][haskell-set-docs]. I see this pattern in many other data structures, both in the API of a specific data structure, and also the variety of them readily available.

[javascript-set-methods]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set#instance_methods
[haskell-set-docs]: https://hackage.haskell.org/package/containers-0.6.7/docs/Data-Set.html

I understand the reason - JavaScript interpreters natively implement the core data structures according to the spec, and anything on top of them that is implemented on the JavaScript side is likely to be a lot slower.
I believe that is one of the main reason libraries like [immutable.js][] is not used more often.

[immutable.js]: https://immutable-js.github.io/immutable-js/

There is another detail worth mentioning here is that the JavaScript Set is usually (always?) a hash table. In Haskell-land, we have two major packages that provide a Set data type: `containers` and `unordered-containers`, and they provide tree-based and hash-based containers respectively. Both are super widely used, but I usually go with `containers` because tree-based structures tend to provide an interface better suited for functional programming. In this case, `alterF` is a good example of that. But it does mean that if I were to go with `unordered-containers`, my implementation here would look more similar to the JavaScript version.

### Loops vs folds

<div class="pure-g">
<div class="pure-u-1 pure-u-md-1-2"><div style="padding-right: 1px">

```javascript
let p = 1;
let seen = new Set();
for (let value of list) {
  ...
}
return seen.size / p;
```

</div></div>
<div class="pure-u-1 pure-u-md-1-2"><div style="padding-left: 1px">

```haskell
 (seen, p) <- foldM go (Set.empty, 1) xs
 return $ fromIntegral (Set.size seen) / p
where
  go (seen, p) value = do
    ...
```

</div></div>
</div>

This is what most people think of when they think of functional programming, using higher-order functions insteads of loops. 

I guess it's a choice of style, I like that the fold makes the structure of the iteration explicit. However, after working with colleagues more experienced with JavaScript, I can now see that the fold makes the "execution order" of the code less obvious. I think it's a Haskeller habit that we do not usually think of the code as a sequence of instructions, so the execution order being different from the order of the code is less of a problem for us. 

---

In the end, I guess there's not much to say other than "Haskell code is fancier". In this example the fanciness actually has many benefits - whether it is worth it or not depends on the situation.

This is pretty much how much time I'm willing to spend writing about this. If you made it this far, thanks!