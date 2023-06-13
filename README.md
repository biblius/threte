# Threte

Implementation of the Rete algorithm in rust.

The Rete implementation closely follows [this amazing paper](http://reports-archive.adm.cs.cmu.edu/anon/1995/CMU-CS-95-113.pdf).

## Todo

- Rete
  - [x] Network
  - [] Node Unlinking
  - [] In place WME modification
  - [] Lazy matching
  - [] Flavor of rete with collection oriented match (use trait to represent wme?)
  - [] Check performance with raw pointer vs Rc\<RefCell>>
- Engine
  - [] Rules
  - [] Rete bridge
  - [] Production queue
  - [] Benchmarks
  - [] Visualiser
  - [] Examples

The long term goal is to create a working rule engine a la Drools, using rust macros as the main method for writing rules.

An important sanity note on negative and NCC nodes:

"The function for creating new negative nodes is similar to the ones for creating beta memories
and join nodes. However, one additional consideration is important with negative conditions,
and also with conjunctive negations. Any time there is a variable \<v> which is tested in a negative
condition and bound in one or more other (positive) conditions, at least one of these positive
conditions must come before the negative condition. Recall that when we add a production to
the network, the network-construction routines are given a list of its conditions in some order. If all conditions are positive, any order will work. Negative conditions require the aforementioned
constraint on the order, though, because negative nodes need to be able to access the appropriate
variable bindings in tokens, and the tokens "seen" by negative nodes indicate only variable
bindings from earlier conditions, i.e., conditions higher up in the network."
