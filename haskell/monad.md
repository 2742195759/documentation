[TOC]

## 背景

这个文档专门为了理解 haskell 的 monad 的概念而编写的。起源于我们需要完成一个组合任务：实现一个 Pipeline 程序，可以将管道中的String进行操作，包含追加和消耗两个操作。

- 追加：给管道中的剩余数据添加一个String

- 消耗：给管道中的数据按照某个方式读取，消耗某一部分String

即一个类似 队列 的数据结构，只不过我们希望使用 Monad + do 的语法来实现简单的数据处理和解析。

上述的问题在 TCP 链接中很常见，因为 TCP 链接需要进行读取到某个空行，所以进行读取是很重要的。而需要考虑阻塞的问题，所以需要一个 BufferedString 来进行处理。

## 思考

### 组合的定义和Monad结构的定义

Monad 之前在 Category for programmer 一书中看到了一个 Log 的例子，而且也已经理解了。所以我们可以很简单的写下下面的组合来实现一个 Log 追加的程序：

```haskell
instance Functor Logger
instance Applicative Logger
instance Monad Logger where
    return x = Logger x ""

    {-(>>=) :: Logger a -> (a -> Logger b) -> Logger b-}
    (Logger va la) >>= f = let Logger vb lb = f va in 
                                Logger vb (la ++ lb)

```

模仿上述的代码，我们可以写一些代码来实现状态在Monad中的封装：

```haskell
type Buffer = String
data Pipeline a = Pipeline Buffer a 

instance Functor Pipeline
instance Applicative Pipeline
instance Monad Pipeline where
    return x = Pipeline "" x
    (>>=) x f = ???
                   
setBuffer :: String -> Pipeline ()

getLine :: Pipeline String

getBuffer :: Pipeline a -> String
getBuffer (Pipeline buf a) = buf
```
上述代码列出了几个操作，用来封装操作。这些操作都应该是 a -> Pipeline b 的签名，否则无法在 do 语句中进行组合。
但是这里考虑 getLine （消耗一行）会发现一些问题：

- getLine 应该没有输入，同时应该将上一个的 Pipeline 按照某个方式进行削减才对。但是这里 getLine 的实现是没法削减上一个Pipeline的。

更进一步的解决上述的问题，可以有下面的解决方案：

- getLine 如果返回一个函数，同时修改 Monad 的组合方式，成为每个Step返回自己处理State的Action，并且在最后添加一个 run 来进行 action 的调用。这样可以实现状态的转移，并且可以很好的组合Monad。

显然返回 Action 是一个表示能力更强的方案，这也是Parser Combinator 的解决方案。（解释了为啥返回值一定是一个 action）因为 State 的组合是靠 Monad 中的 (>>=) 来实现的，如果直接每个函数返回新的状态，那么每个函数还需要知道上一个状态，了解的知识就太多了。当前函数应该只考虑自己的主要逻辑和影响函数。将状态之间的变化交给组合逻辑来实现。

Log 之所以可以这么简单，是因为他们的组合固定为 log 的 ++，即将逻辑上放到组合层，自己只返回必要的数据结构。然后数据的使用用 >>= 即可，但是如果操作本来就复杂，那么直接返回函数是表示能力更强的方案。注意，最小知识量原则。只返回函数需要关心的，比如这里的当前Buffer的值即可。


### 问题2： 如何存储函数

这里遇到了另外的一个问题，因为我们希望使用函数来作为 Monad 的状态，所以我们应该是类似这样的结果：

```haskell
data Context a = Context Function a
```

这里的 Function 是上述的 `String -> String` 的处理函数，那么我们因该怎么做呢。最后借鉴了 Parsec 中的定义：
```haskell
newtype ParsecT s u m a
    = ParsecT {unParser :: forall b .
                 State s u
              -> (a -> State s u -> ParseError -> m b) -- consumed ok
              -> (ParseError -> m b)                   -- consumed err
              -> (a -> State s u -> ParseError -> m b) -- empty ok
              -> (ParseError -> m b)                   -- empty err
              -> m b
             }
```
即，我们无法使用 type 来定义一个完全兼容的函数类型，但是可以使用 data 或者是 newtype 定义一个自定义的类型，然后这个自定义的类型可以包含一个函数类型。这样的话，我们就可以实现函数的封装。也可以将函数封装到一个 Monad 中作为函数的返回值了。拨错捏。
