# Threte

A [rule engine](https://en.wikipedia.org/wiki/Business_rules_engine#:~:text=Rules%20engines%20or%20inference%20engines,the%20need%20for%20IT%20intervention.) implementation in R*st.

The engine consists of a production queue and a pattern matcher implemented using the [Rete algorithm](https://en.wikipedia.org/wiki/Rete_algorithm).

The Rete implementation closely follows [this amazing paper](http://reports-archive.adm.cs.cmu.edu/anon/1995/CMU-CS-95-113.pdf).

## Todo

- Rete
  - [ ] Change the mechanism with which children are removed (try indexmap instead of Vec::retain as the latter can waste work)
  - [x] Network (Alpha, Beta, Join, Negative, NCC nodes)
  - [x] Node Unlinking
  - [ ] Non-equality join tests
  - [ ] In place WME modification
  - [ ] Lazy matching
  - [ ] Flavor of rete with collection oriented match (use trait to represent wme?)
  - [ ] Investigate possible optimisations with MaybeUninit
  - [ ] Check performance with raw pointer vs Rc\<RefCell\<T>>
- Engine
  - [ ] Rules
  - [ ] Rete bridge
  - [ ] Production queue
- Misc
  - [ ] Benchmarks
  - [ ] Simple Rete Visualiser
  - [ ] Examples

The implementation is far from production ready and can be improved in many ways.

## An important sanity note on negative and NCC nodes

"The function for creating new negative nodes is similar to the ones for creating beta memories
and join nodes. However, one additional consideration is important with negative conditions,
and also with conjunctive negations. Any time there is a variable \<v> which is tested in a negative
condition and bound in one or more other (positive) conditions, at least one of these positive
conditions must come before the negative condition. Recall that when we add a production to
the network, the network-construction routines are given a list of its conditions in some order. If all conditions are positive, any order will work. Negative conditions require the aforementioned
constraint on the order, though, because negative nodes need to be able to access the appropriate
variable bindings in tokens, and the tokens "seen" by negative nodes indicate only variable
bindings from earlier conditions, i.e., conditions higher up in the network."

## Motivation

Rete and Rust are cool and fun, also something something AI world domination
