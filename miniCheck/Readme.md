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
transition_system = definition | definition newline transition_system;
definition        = initial_state | normal_state | actions | transition | label_func;
initial_state     = "initial state" identifier {"," identifier};
normal_state      = "state" identifier {"," identifier};
transition        = "trans" identifier (identifier | "TRU€") identifier;
label_func        = "labels" identifier ":" label {"," label}
label             = ["-"] identifier

newline           = "\n";
lower_char        = "a" | "b" | "c" | ... | "z";
identifier        = lower_char {"_" | lower_char};
```

Example:

```
initial states pay
states select, soda, beer
actions insert_coin, get_beer, get_soda

tr️ans soda get_soda pay
trans beer get_beer pay
trans pay insert_coin select
trans select TRUE beer           //internal transition
trans select TRUE soda

labels select: -x, y, -z, -a, b
```

## Computational Tree Logic (CTL)
