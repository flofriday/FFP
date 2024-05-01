# MiniCheck

## Run

```
cabal run
```

## Transition Systems (TS)

Formally, a TS is a tuple $(S, Act, \to, I, AP, L)$, where

- $S$ is a set of _states_,
- $Act$ is a set of _actions_,
- $\to \subseteq S x Act x S$ is a _transition relation_
- $I \subseteq S$ is a set of initial states
- $AP$ is a set of _atomic propositions_
- $L : S \to 2^{AP}$ is a labelling function.

### Text Representation

![TS example image](ts_example.png)

- states: _pay_, _select_, _soda_, _beer_
- actions: _get_soda_, _get_beer_, _insert_coin_

```
<transition system> ::= ε | <initial state> | <transition> | <transition system>
<initial state>     ::= <transition> <state> <newline>
<normal state>      ::= <state> <transition> <state> <newline>
<transition>        ::= - <action> ->
<action>            ::= <lowerCharSeq>
<state>             ::= <lowerCharSeq>

<newline>           ::= \n
<lowerChar>         ::= a | b | c | ... | z
<lowerCharSeq>      ::= ε | <lowerChar><lowerCharSeq>
```

Example:

```
--> pay
pay -insert_coin-> select
select -insert_coin-> select
```

## Computational Tree Logic (CTL)
