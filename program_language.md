# 抽象新的语法

抽象一门新的语法一直都让人很容易忘记，但是在不同的方向，有的语言就是更加常用，也有更加多的库来强化这门语言，这就让掌握多门语言很有必要。那么怎么快速记住这些多样的语法呢？我们使用抽象的观点来解释他们。

# 按照需求进行语法的记录

## 数组相关

构造数组：

``` C++
std::vector<T> arr;
```

```haskell
# 初始化
a = [1,2,3] 
# 构造函数
a = 1 : [2, 3]
```

```javascript
a = [1,2,3]
```

数组遍历：

```C++
for (auto i in arr) {
    # xxx
}
```

```haskell
# 递归替代遍历
add [] = 1
add x: xs = x + add xs
```

数组的map : 


## Web处理和Web爬取


## 文件读写和文件路径


## 网络编程 + Socket


## UI 编程


## Haskell Related

### 自定义类型

`data` 语法是用来构建新的对象，如下代码: 

```haskell
data MyString = MyString String Int 
                    deriving (Show, Eq)

```
上文定义了一个笛卡尔积。通过 type 可以更加方便的使用代数数据类型：
```haskell
newtype MyString = (String, Int)
                    deriving (Show, Eq)

```
通过上述的例子，我们可以思考类型构造器和值构造器的区别。

- 类型构造器：是类型的函数（代数数据类型）

- 值构造器：是每个类型中值的函数

所以他们是两个不同维度但是相互关联的东西，如果将类型作为一个SET范畴，那么类型构造器是类型范畴中的<font color='red'>Hom-Set</font>。而值构造器就是HomSet中的每个元素，即每个值构造器就是<font color='red'>Morphism</font>. 

等价数学来考虑：

$$
\begin{align}
    f: \R & \rightarrow \R \\
    f: x  & \rightarrow 2 * x
\end{align}
$$

这里的 f 就是函数属于的Hom-Set，而下面的f就是函数具体的值。第一个就是类型构造器，第二个就是值构造器。

<font color='red'>类型构造器只是笛卡尔乘积的别名</font>。类型构造器只是一个<font color='red'>Type</font>的额外名字而已。

真正的 Generic Type 才是 Type -> Type 的函数。


### OverloadedStrings

官网文档如下：[OverloadedStrings](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html#extension-OverloadedStrings)

默认情况下，一个字符串常量会有String类型，但是如果用户希望在不同的上下文，默认调用不同的构造函数，那么可以使用OverloadedStrings。

如果一个Class是IsString的实例，那么它实现了fromString就可以从String构造出一个Class实例，这样用户不用显示的进行调用。而是直接让String字面量有多态的感觉。

如下代码：

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.String( IsString(..) )

data MyString = MyString String Int deriving (Show, Eq)
instance IsString MyString where
    fromString a = MyString a 1

getString::MyString -> Int
getString (MyString s i) = i

main = do 
    print $ getString "sdfsf"
```
可以看到上述代码中，因为MyString重写了from String，所以在需要获取到 MyString时，编译器会自动调用 fromString 构造函数，实现自动重载。也就是字面量String可以作为任何的类型的构造参数。

如果去掉 OverloadedStrings，我们会得到如下错误：

```log
/root/haskell/test.hs:11:23: error:
    • Couldn't match type ‘[Char]’ with ‘MyString’
      Expected: MyString
        Actual: String
    • In the first argument of ‘getString’, namely ‘"sdfsf"’
      In the second argument of ‘($)’, namely ‘getString "sdfsf"’
      In a stmt of a 'do' block: print $ getString "sdfsf"
   |
11 |     print $ getString "sdfsf"
   |                       ^^^^^^^

```
