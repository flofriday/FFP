## Important

- Implement extension 1
- Implement extension 2
- Implement Bonus task
- Fix Parsing
  - modify ts so there is no terminal state
  - check if all states are present
  - Set all APs that are not mentioned in a label mapping to false
  - check that labeling functions sets own AP to TRUE
  - at least one initial state

## Optional

- Fix different modes of cli argument parsing, i.e. "extension mode" should just be usable with
  - `cabal run miniCheck -- --extensions` instead of
  - `cabal run miniCheck -- extensionmode --extensions`
