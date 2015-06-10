# Real World Haskell Ch. 10, Explained

**Real World Haskell** is a great book for learning Haskell, but one
chapter, Chapter 10, is notorious for giving beginners a hard time. It
introduces important concepts in a very roundabout way, with
convoluted code examples that in some cases don't even run. This post
is attempted as an understanding aid for that chapter, for me and for
Haskell beginners who get stuck in that chapter. To be perfectly
honest, understanding this chapter completely is not necessary; one
could just skip to the section on functors without missing much. This
write-up is simply to satisfy my obsessiveness.

Chapter 10 uses as example the parsing of [PGM (portable grey
map)](http://en.wikipedia.org/wiki/Netpbm_format) files. A sample file
in this ancient but simple format is included in this repo as
`test.pgm`. Here is what you will see if you open this file in a text
editor:

    P5
    640 480
    255
    \377\377...

The last row is binary data that specifies greyscale values for
individual pixels. The representation of a parsed PGM image generated
by the example parsers in this chapter is straightforward; a data type
named Greymap with fields grepWidth, greyHeight, greyMax and greyData
is used. In order to represent such an image properly on the console,
the `Show` interface of a Greymap is implemented to print size and
maximum greyscale information. Since the Greymap data type is used by
all code examples, it is included in this repo in the file `common.hs`
imported by the other files. Here is the Greymap definition from that
file:

```haskell
data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    } deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++
                             " " ++ show m
```

Given a file in PGM format, how should we read it into memory? Since
the file contains mostly binary data, we can't (or shouldn't) use the
Prelude methods that read a file into a string. Instead, we should
read the file contents into a
[`Data.ByteString.Lazy.ByteString`](https://hackage.haskell.org/package/bytestring-0.9.1.5/docs/Data-ByteString-Lazy.html#t:ByteString)
. The methods to do that, and also many other methods to deal with
binary data, are in two modules:
[`Data.ByteString.Lazy.Char8`](https://hackage.haskell.org/package/bytestring-0.9.1.7/docs/Data-ByteString-Lazy-Char8.html)
and
[`Data.ByteString.Lazy`](https://hackage.haskell.org/package/bytestring-0.9.2.1/docs/Data-ByteString-Lazy.html). These
will be imported as L8 and L, respectively, to avoid excessive typing.

## Linear parsing

The PGM format is simple to parse; the parser can linearly go through
the file, matching the next piece of data into whatever is expected,
and stopping if something unexpected happens. In the first example for
parsing, the authors do exactly this. A working version of their code
can be found in `pgm1.hs`; you can compile this file with `ghc
pgm1.hs`, and run it with `./pgm1 test.pgm`. This will print the
result of the `show` call on the Greymap parse result.

Let's have a look at how the parsing is carried out. The parser has to
look for three kinds of data: header, integers, and the image
bytes. These are done with the `matchHeader`, `getNat` (for natural
number), and `getBytes` methods. Here's the `matchHeader` function:

```haskell
matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise
        = Nothing
```

Keep in mind that `L.ByteString` here is the
[`Data.ByteString.Lazy.ByteString`](https://hackage.haskell.org/package/bytestring-0.9.1.5/docs/Data-ByteString-Lazy.html#t:ByteString)
data type. The above function can be used to check whether a
ByteString starts with the given prefix, returning `Nothing` if
not. If it does, the prefix and any following whitespace is dropped
(using
[`Data.ByteString.Lazy.Char8.dropWhile`](https://hackage.haskell.org/package/bytestring-0.9.1.7/docs/Data-ByteString-Lazy-Char8.html#v:dropWhile)
and `isSpace`), and returned with a `Just`. Observe that the second
argument of this function is also a `ByteString`; we will see later
how a string can be packed into a `ByteString` so that it can be
passed to this function appropriately.

The next function is for matching an integer:

```haskell
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
             Nothing -> Nothing
             Just (num,rest)
                 | num <= 0    -> Nothing
                 | otherwise -> Just (fromIntegral num, rest)
```

This function does something very similar to the previous one; it
picks the piece of interesting data from the start of the string,
using the
[`Data.ByteString.Lazy.Char8.readInt`](https://hackage.haskell.org/package/bytestring-0.9.1.7/docs/Data-ByteString-Lazy-Char8.html#25)
function. In the case of error (`readInt` returns `Nothing` if number
is less than 0), `Nothing` is returned. In case of success, the parsed
number and the rest of the `ByteString` are packed into a tuple and
returned as a `Just`.

The last function that fetches bytes, `getBytes`, is different in that
it takes as an argument how many bytes to read. This is computed from
the size of the image parsed earlier:

```haskell
getBytes :: Int -> L.ByteString
         -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count           = fromIntegral n
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both
```

`getBytes` splits the `ByteString` into a prefix of the given size and
the rest using
[`Data.ByteString.Lazy.splitAt`](https://hackage.haskell.org/package/bytestring-0.9.1.7/docs/Data-ByteString-Lazy.html#v:splitAt). The
size of the prefix (determined using
[`Data.ByteString.Lazy.length`](https://hackage.haskell.org/package/bytestring-0.9.1.7/docs/Data-ByteString-Lazy.html#v:length)
is compared to the count argument, and if it is smaller, which means
that the `ByteString` was not big enough, `Nothing` is
returned. Otherwise, the tuple of the prefix and rest data is
returned.

The parsing function that uses these matchers is very
straightforward. It applies the matchers consequently, returning
`Nothing` if one of them returns `Nothing`, otherwise ending with the
`Greymap` instance that packs the data that was parsed.
[`Data.ByteString.Lazy.Char8`](https://hackage.haskell.org/package/bytestring-0.9.1.7/docs/Data-ByteString-Lazy-Char8.html#v:pack)
is used to turn the PGM prefix P5 into a `ByteString`, passing the
result into `matchHeader`. If the return value is `Nothing`, parsing
ends. Otherwise, the returned rest `ByteString` is destructured from
the `Just`, and fed to `getNat` to fetch the width. The parsing goes
on like this, as the ByteString is consumed piece by piece, and the
parsed data is gathered in the nested function scopes. If we run the
command `./pgm1 test.pgm` on the command line, here's the output we
should see:

    Greymap 640x480 255

which is exactly what we should expect.

As straightforward as the parsing function is, it's not good
code. It's very repetitive, would be difficult to change and adapt to
any variations in file format, does not offer abstraction, and looks
plain ugly. The first step in refactoring the parsing function is
factoring out the chaining, where `Nothing` from a matching function
leads to exit, whereas a `Just` value leads to the contents getting
passed on to the next step. This is done using a binary operator `>>?`
that encapsulates this logic:

```haskell
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v
```

Observe that this is an infix function, and it's left-associative like
any other function, so `a >>? b >>? c` is equivalent to `(a >>?  b)
>>? c`. The parsing function that uses this combination operator looks
like this:

```haskell
parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
    matchHeader (L8.pack "P5") s      >>?
    \s -> skipSpace ((), s)           >>?
    (getNat . snd)                    >>?
    skipSpace                         >>?
    \(width, s) ->   getNat s         >>?
    skipSpace                         >>?
    \(height, s) ->  getNat s         >>?
    \(maxGrey, s) -> getBytes 1 s     >>?
    (getBytes (width * height) . snd) >>?
    \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)
```

This is from the file `pgm2.hs`, which you can compile and run, as
with `pgm1.hs`, on `test.pgm`. A method `skipSpace` had to be added to
account for the clean up done within the original parseP5 function:

```haskell
skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)
```

Also pay attention to the `(getNat . snd)` combination, which serves
to pick the second element of the tuple returned by skipSpace and feed
it to `getNat`. The execution of the chained functions built by the
`parseP5_take2` method follows from top to bottom in a wrapped
fashion. That is, the last line is the outermost function application.
As the chain unfolds from the first function on, if one of the
functions returns `Nothing`, that is passed on to the end.

There is one very tricky thing about `parseP5_take2`, though. In
`parseP5`, the parsed integer values (`width`, `height`, and
`maxGrey`) are stored in the closures of successive function calls. At
first impression, the same thing should happen within parseP5_take2,
since there is no explicit management of these variables, but the
indentation tells us that the lambdas are what get chained. But these
lambdas have separate function contexts, so how are they supposed to
contribute to the common closure? They can't and they don't; it's not
the lambdas that are getting chained, but the function calls within
the lambdas. You can see this more clearly if you wrap one of the
lambdas in paranthesis (e.g. the one on line 54) and try to compile
again, which returns the following error:

```
$ ghc pgm2.hs
[1 of 1] Compiling Main             ( pgm2.hs, pgm2.o )

pgm2.hs:58:16: Not in scope: ‘width’

pgm2.hs:59:35: Not in scope: ‘width’
```

Each lambda is a wrapper around the rest of `parseP5_take2`. The
correct (or rather more truthful) indentation would be the following:

```haskell
parseP5_take2_truthful :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2_truthful s =
    matchHeader (L8.pack "P5") s      >>?
    (\s -> skipSpace ((), s)           >>?
      (getNat . snd)                    >>?
      skipSpace                         >>?
      \(width, s) ->   getNat s         >>?
        skipSpace                         >>?
        \(height, s) ->  getNat s         >>?
          \(maxGrey, s) -> getBytes 1 s     >>?
            (getBytes (width * height) . snd) >>?
            \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)
```

which doesn't look as nice as the previous one.

Since the second variation does not really solve the problem of
encapsulating parse state, and is also misguiding in the way it's
built, time for a third variation. From this point on, things get a
bit weird. The third example uses rather convoluted code to insinuate
at the idea of functors, which doesn't really work. This leads to code
that's difficult to understand at a syntactic level, and even if you
understand it, doesn't make sense at a functional level. For this
reason, and to keep my sanity, I have refrained from going too deep
down the rabbit hole.

In the third variation, the parse state is packed inside the type
`ParseState` that contains the complete `ByteString` and the current
offset:

```haskell
data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64           -- imported from Data.Int
    } deriving (Show)
```

The parsing function will take a `ParseState` with the `ByteString` to
be parsed and the offset set to 0, returning an `Either String (a,
ParseState)`. The question is how to encapsulate the parsing
steps. The authors use a newtype declaration to achieve this. This is
a surprise move, as there was no example of putting a function inside
a newtype constructor, and it is one of the ???. Here's a sample that
you can copy-paste into ghci to help you understand it:

```haskell
newtype IntOperation a = IntOperation { process :: a -> Either String a }

let multiplier limit = IntOperation $ \arg -> if arg > limit then (Left "Int too big") else Right (arg * 10)
```

If you then run `(process (multiplier 2)) 5` in ghci, you should see
the output `Left "Int too big"`. `IntOperation` is simply a wrapper
around a lambda passed in with the `process` parameter. This lambda,
together with the wrapping `IntOperation`, can be created using
something like a factory method, in this case `multiplier`.

Here is the parsing function encapsulation from the book:

```haskell
newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }
```

Haskell being a functional language, this same thing could have been
achieved using a simple closure. The only advantage of this
formulation is that new types can be created only using the `Parse`
constructor; using a function, one would have to declare a function
type such as `ParseState -> Either String (Int, ParseState)`. The big
mystery for me here was what the heck the `a` argument here is -- one
point where the Haskell pattern of single-letter argument names
fails. The use of the `Parse` type in the parsers that follow in this
chapter reveal that `a` stands for the output of the previous parsing
step, such as an int or a byte.

A sample parser where we can see the Parser type in action is the
identity parser which returns whatever it is given. Here are the
relevant lines from `parse_identity.hs`:

```haskell
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))
```

So whatever `ParseState` it is given (`s` in the encapsulated lambda),
identity will return whatever it was initialized with and the
`ParseState` in a tuple. This parsing element will be used in a sample
parser in this chapter.

Parsing a file involves reading data from the `ByteString` and
increasing the offset by the number of bytes read. Increasing the
offset of a `ParseState` is done using the bracket notation on records
to create new ones:

```haskell
modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset =
    initState { offset = newOffset }
```

The output of this function is a new ParseState that is different only
on the `offset` field from `initState`.

Another thing we need is a way to chain `Parser`s. That is, we need
the capability to take one `Parse`, run it (remember, a `Parse`
encapsulates parsing logic in a lambda), take the result, and put it
into another `Parse`. If you look at the type definition of `==>` from
`pgm3.hs`, this is exactly what it does:

```haskell
(==>) :: Parse a -> (a -> Parse b) -> Parse b
```

When used as a binary operator, the left argument will be a `Parse a`,
where a is the type wrapped in `Either` returned by the lambda within
`Parse`. The right will be a function that takes an `a` as an
argument, and returns a `Parse b`. The definition of this operator is
relatively straightforward:

```haskell
firstParser ==> secondParser  =  Parse chainedParser
  where chainedParser initState   =
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState
```

Let's try to understand what's happening here, shall we? The result of
the `==>` operator is the `chainedParser` function that is created in
the `where` clause, wrapped in a Parse. Before the `runParse` function
is called on the end result and the unwrapped function is called, this
function will not run, so we're essentially creating state wrapped in
a lambda inside a newtype. `chainedParser` gets an `initState` as an
argument, as it should since it's getting wrapped in a Parse. It then
runs the `firstParser` on this state. If the result is an error
message, the output of `chainedParser` is also an error message. If
the result is `Right (firstResult, newState)`, the `secondParser` is
initialized with `firstResult`, and called with `newState`. The main
difficulty in understanding this function stems from `firstParser` and
`secondParsers` having names that imply they are of the same kind, but
they actually aren't. `firstParser` is `Parse a`, whereas
`secondParse` is `a -> Parse b`, i.e. a factory that produces a
`Parser`.

Now let's look at the actual parser from the third example that uses
the above tools:

```haskell

bail :: String -> Parse a
bail err = Parse $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err

parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing ->
          bail "no more input"
      Just (byte,remainder) ->
          putState newState ==> \_ ->
          identity byte
        where newState = initState { string = remainder,
                                     offset = newOffset }
              newOffset = offset initState + 1
```

`bail` here is a very simple function to return an error. `parseByte`
uses two functions we haven't seen yet, `getState` and `putState`.
These are both a bit weird; `getState` is a `Parse` that puts the
state it is passed in both parts of the returned `Either`:

```haskell
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))
```

It is used in `parseByte` to pack the initial state for the next
step. `putState` returns unity and the parse state as a result:

```haskell
putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))
```

Here is how `parseByte` works. The central piece is the `==>` operator
that chains a `Parse` with a `Parse` factory (`a -> Parse b`) to
create another `Parse`. The chained elements are `getState` with the
lambda that follows it, and then within the lambda, `putState` with a
lambda that creates an `identity Word8`. When `parseByte` is called on
a `ParseState` created from the contents of the `test.pgm` file (lines
67 and 68 in `pgm3.hs`), the combined parser created by the first
`==>` is fired. It runs `getState`, which returns the exact same
initial `ParseState` in both slots of a tuple. The second argument to
`==>` in this first chain, the lambda starting at line 49 with the
single argument `initState`, is run with the first slot of the tuple,
the initial state. This lambda takes the `ByteString` and uncons's it
using
[`Data.ByteString.Lazy.uncons`](https://hackage.haskell.org/package/bytestring-0.9.2.1/docs/Data-ByteString-Lazy.html#v:uncons)
-- that is, splits head and tail. If there was no data, `bail` is
called. Otherwise, another chain is created using `==>`. This one
involves `putState` and another lambda. As we already know, `putState`
takes whatever it was given, and creates a `Parse` that returns a
unity and that thing. When we couple this with an `identity` that
receives as its first argument the byte that was parsed in the
containing lambda, and returns that byte no matter what it is called
with, what we get is the byte. To make this last step more
understandable, here is a step-by-step evaluation:

```haskell
(putState newState) ==> \_ -> identity byte
-- is equivalent to
Parse (\_ -> Right ((), newState) ==> \_ -> Parse (\s -> Right(byte, s))
-- is equivalent to
runParse ( Parse (\s -> Right(byte, s)) newState
-- is equivalent to
Right (byte, newState)
```

We end up with the remainder from the `uncons` operation and a new
incremented offset in a `ParseState`, and the parsed byte. `pgm3.hs`
tells me that the first byte of `test.pgm` is 80 when I run it.

There are a number of obvious problems with `parseByte`. First of all,
`getByte` and `putByte` are totally senseless; we don't need them to
pass a `ParseState` to the `case` statement they enclose. They are
used just to demonstrate the `==>` operator; it would be relatively
easy (but still pointless) to write a `parseByte` alternative without
any of these two functions. Also, what's the deal with the weird
combination of `putState` and `identity`? They are essentially just
swapping arguments and returning a value in a backhanded manner
through a closure. `parseByte` is not how one should write Haskell.

## Parsing with functors

The discussion of functors in Chapter 10 is rather straightforward and
the examples do not need explanation, so we will skip that. The only
thing I would like to note is the definition of a functor, which is a
class with one method:

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

The type `f` here is referred to as the container type. There are two
rules functors must obey to be considered a proper implementation. The
first is that a functor must preserve identity:

```haskell
fmap id ==  id
```

The second is that functors must be composable, i.e. applying the
composition of two functions to a functors should be the same as
applying them separately:

```haskell
fmap (f . g)  ==  fmap f . fmap g
```

And now for the fourth iteration of the PGM parser using functors. For
this iteration, `Parse` turned into a functor, and then chained using
a new combination operator. The functor instance definition is as
follows:

```haskell
instance Functor Parse where
    fmap f parser = parser ==> \result ->
                    identity (f result)
```

It's the same identity trick that we saw earlier. What does this
combinator do? It returns a `Parser` that, when evaluated with
`runParser` on a `ParseState`,