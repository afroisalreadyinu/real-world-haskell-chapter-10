# Real World Haskell Ch. 10, Explained

**Real World Haskell** is a great book for learning Haskell, but one
chapter, Chapter 10, is notorious for giving beginners a hard time. It
introduces important concepts in a very roundabout way, with
convoluted code examples that in some cases don't even run. This post
is attempted as an understanding aid for that chapter, for me and for
Haskell beginners who get stuck in that chapter.

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
by the example parsers in this chapter is straightforward; an
algebraic data type named Greymap with fields grepWidth, greyHeight,
greyMax and greyData is used. In order to represent such an image
properly on the console, the `Show` interface of a Greymap is
implemented to print size and maximum greyscale information. Since the
Greymap data type is used by all code examples, it is included in this
repo in the file `common.hs` imported by the other files. Here is the
Greymap definition from that file:

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
the file contains mostly binary data, we can't use the Prelude methods
that read a file into a string. Instead, we should read the file
contents into a
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
bytes. These are done with the matchHeader, getNat (for natural
number), and getBytes methods. Let's have a look at the first one:

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
ByteString starts with the given prefix, returning Nothing if not. If
it does, the prefix and any following whitespace is dropped (using
[`Data.ByteString.Lazy.Char8.dropWhile`](https://hackage.haskell.org/package/bytestring-0.9.1.7/docs/Data-ByteString-Lazy-Char8.html#v:dropWhile)
and `isSpace`), and returned with a Just. Observe that the second
argument of this function is also a ByteString; we will see later how
a string can be packed appropriately.

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
function. In the case of error (readInt returns Nothing or number is
less than 0), Nothing is returned. In case of success, the parsed
number and the rest of the ByteString are packed into a tuple and
returned with Just.

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

`getBytes` splits the ByteString into a prefix of the given size and
the rest using
[`Data.ByteString.Lazy.splitAt`](https://hackage.haskell.org/package/bytestring-0.9.1.7/docs/Data-ByteString-Lazy.html#v:splitAt). The
size of the prefix (determined using
[`Data.ByteString.Lazy.length`](https://hackage.haskell.org/package/bytestring-0.9.1.7/docs/Data-ByteString-Lazy.html#v:length)
is compared to the count argument, and if it is smaller, which means
that the ByteString was not big enough, Nothing is
returned. Otherwise, the tuple of the prefix and rest data is
returned.

The parsing function that uses these matchers is very
straightforward. It applies the matchers consequently, returning
Nothing if one of them returns Nothing, otherwise ending with the
Greymap instance that packs the data that was parsed.
[`Data.ByteString.Lazy.Char8`](https://hackage.haskell.org/package/bytestring-0.9.1.7/docs/Data-ByteString-Lazy-Char8.html#v:pack)
is used to turn the PGM prefix P5 into a ByteString, passing the
result into `matchHeader`. If the return value is Nothing, parsing
ends. Otherwise, the returned rest ByteString is destructured from the
Just, and fed to `getNat` to fetch the width. The parsing goes on like
this, as the ByteString is consumed piece by piece, and the parsed
data is gathered in the nested function scopes. If we run the command
`pgm1 test.pgm` on the command line, here's the output we should see:

    Greymap 640x480 255

which is exactly what we should expect.

As straightforward as the parsing function is, it's not good
code. It's very repetitive, would be difficult to change and adapt to
any variations in file format, does not offer abstraction, and looks
plain ugly. The first step in refactoring the parsing function is
factoring out the chaining, where Nothing from a matching function
leads to exit, whereas a Just value leads to the contents getting
passed on to the next step. This is done using a function `>>?` that
encapsulates this logic:

```haskell
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v
```

Observe that this is an infix function, and it's left associative like
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
parseP5_take2 method follows from top to bottom in a wrapped
fashion. That is, the last line is the outermost function application;
as the chain unfolds from the first function on, if one of the
functions returns Nothing, that is passed on to the end.

There is one very tricky thing about parseP5_take2, though. In
parseP5, the parsed integer values (width, height, and maxGrey) are
stored in the closures of successive function calls. At first
impression, the same thing should happen within parseP5_take2, since
there is no explicit management of these variables, but the
indentation tells us that the lambdas are what get chained. But these
lambdas have separate function contexts, so how are they supposed to
contribute to the common closure? They can't and they don't; it's not
the lambdas that are getting chained, but the function calls within
the lambdas; you can see this more clearly if you wrap one of the
lambdas in paranthesis (e.g. the one on line 53) and try to compile
again, which returns the following error:

```
ulas@kittie-2:~/Documents/documents/notes/for_blog/haskell [master*]$ ghc pgm2.hs
[1 of 1] Compiling Main             ( pgm2.hs, pgm2.o )

pgm2.hs:57:16: Not in scope: ‘width’

pgm2.hs:58:35: Not in scope: ‘width’
```

Each lambda is a wrapper around the rest of the function. The correct
(or rather more truthful) indentation would be the following:

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

Which doesn't look as nice as the previous one.

Since the second variation does not really solve the problem of
encapsulating parse state, and is also misguiding in the way it's
built, time for a third variation.

In this third variation, the parse state is handily packed inside an
algebraic type named ParseState that contians the complete ByteString
and the current offset:

```haskell
data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64           -- imported from Data.Int
    } deriving (Show)
```

The parsing function will take a ParseSTate with the ByteString and
the offset set to 0, returning an `Either String (a, ParseState)`. The
question is how to encapsulate the parsing steps. The authors use a
newtype declaration to achieve this. This is a surprise move, as there
was no example of putting a function inside a newtype constructor, but
given that Haskell is a functional language, it's not so
surprising. Here's a sample that you can copy-paste into ghci to help
you understand it:

```haskell
newtype IntOperation a = IntOperation { process :: a -> Either String a }

let multiplier limit = IntOperation $ \arg -> if arg > limit then (Left "Int too big") else Right (arg * 10)
```

If you then run `(process (multiplier 2)) 5` in ghci, you should see
the output `Left "Int too big"`. IntOperation is simply a wrapper
around a lambda with the process parameter. This lambda, together with
the wrappinmg IntOperation, can be created using something like a
factory method, in this case `multiplier`.

Here is the parsing function encapsulation from the book:

```haskell
newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }
```

As you can see, this is just a way of putting a function inside a
newtype. The advantage here is that we have fixated the input type to
`ParseState`, and make sure that the output of the encapsulated
function is also of a certain type, parametrized by the argument to
the `Parse` type constructor (??). The big mystery for me here was
what the heck the `a` argument here is -- one point where the Haskell
pattern of single-letter argument names fails. The use of the `Parse`
type in the parsers that follow in this chapter reveal that `a` stands
for the output of the previous prasing step, such as an int or a byte.

A sample parser where we can see the Parser type in action is the
identity parser which returns whatever it is given. As can be seen in
the file `parse_identity.hs`, the identity parser can be defined as
follows:

```haskell
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))
```

So whatever `ParseState` it is given (`s` in the encapsulated lambda),
identity will save that initial state, and the then return a tuple
with the

 ??? WTWTWTWTWW

What we need to parse a file is to be able to increase the offset.
This is achieved using the following function that makes use of
applying bracket notation to ADT types to create new ones:

```haskell
modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset =
    initState { offset = newOffset }
```

The output of this function is a new ParseState that has changed only
the offset of the original. Next up, we need two more methods to

https://hackage.haskell.org/package/bytestring-0.9.2.1/docs/Data-ByteString-Lazy.html#v:uncons

Another thing we need is a way to chain `Parser`s. That is, we need
the capability to take one `Parse`, run it (remember, a `Parse`
encapsulates parsing logic in a lambda), take the result, and put it
into another `Parse`. If you look at the type definition of `==>`,
this is exactly what it does:

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
is called on the end result, this function will not run, so we're
essentially creating state wrapped in a lambda inside a
newtype. `chainedParser` gets an initState as an argument, as it
should since it's getting wrapped in a Parse. It then runs the
`firstParser` on this state. If the result is an error message, the
output of `chainedParser` is also an error message. If the result is
`Right (firstResult, newState)`, the `secondParser` is initialized
with `firstResult`, and called with `newState`. The main difficulty in
understanding this function stems from `firstParser` and
`secondParsers` having names that imply they are of the same kind, but
they actually aren't. `firstParser` is `Parse a`, whereas
`secondParse` is `a -> Parse b`, that is, a factory that produces a
`Parser`.
