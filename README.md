# A Simple Vending-Machine State Transition System
This is a Haskell implementation of the [formal specification](https://github.com/nkarag/haskell-vending-machine/blob/master/doc/Vending.pdf). of a hypothetical Vending Machine, which is modeled as a *State Transition System (STS)*. This is called an "*executable specification*", because the Haskell implementation matches the mathematical specification as much as possible. The mathematical notation used, as well the formalism for specifying an STS can be found [here](https://github.com/nkarag/haskell-vending-machine/blob/master/doc/small-step-semantics.pdf).

The Vending Machine STS sells sodas and it comprises of an enviroinment, valid states and two signals that trigger the transtition from one valid state to another, namely: a) *Deposit* an amount and b) *Push* the button of the vending machine in order to get a soda. More details you can find in the formal specification above.
