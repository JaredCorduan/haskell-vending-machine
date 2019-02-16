# A Simple Vending-Machine State Transition System
This is a Haskell implementation of the [formal specification](https://github.com/nkarag/haskell-vending-machine/blob/master/doc/Vending.pdf). of a hypothetical Vending Machine, which is modeled as a *State Transition System (STS)*. This is called an "*executable specification*", because the Haskell implementation matches the mathematical specification as much as possible. The mathematical notation used, as well the formalism for specifying an STS can be found [here](https://github.com/nkarag/haskell-vending-machine/blob/master/doc/small-step-semantics.pdf).

The Vending Machine STS sells sodas and it comprises of an enviroinment, valid states and two signals that trigger the transtition from one valid state to another, namely: a) *Deposit* an amount and b) *Push* the button of the vending machine in order to get a soda. More details you can find in the formal specification above.

The basic module Vending (in the src directory) implements the Vending Machine STS. In the test directory you can find the unit tests described in the formal specification document, implemented with the [Tasty](https://hackage.haskell.org/package/tasty) framework.
In order to run the tests, simply clone this repo and then run `stack test`
