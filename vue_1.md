# Vue 自学笔记

## 参考文献

1. [Vue官网教程](https://cn.vuejs.org/tutorial/#step-5)

## 思考：Vue提供了什么便利？

Vue 对于传统的 HTML + CSS + JS 提供了哪些便利？

传统意义上，如果要实现 HTML 和 JS 的双向关联，我们需要

## 逻辑数据与UI事件的同步

## 组件化

编程语言提供的组件化是非常重要的功能，

## 思考：传参 + 返回值 + Event/Emit机制 的本质对比


## [Vue教程有感](https://cn.vuejs.org/tutorial/#step-15)

体验了整个Vue的教程和Vue提供的机制，感触最深的是如下的几点：

- 组件化：每个 vue / js 都可以组织成为一个组件，非常适合模块化。

- 组件消息传递：双向，父子组件的传参方法是
    
    - slot，父 -> 子，传递模板内容

    - prop，父 -> 子，传递状态内容

    - emit / event 机制，子 -> 父，传递回调函数

- 基于发布-订阅者模式构建的View、Data、Controller 模型： 整个 Vue 其实最关键的事情是实现逻辑数据和UI数据的同步。
