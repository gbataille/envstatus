#!/bin/bash

# Does not use cabal new-test because
#   - it does not output in color in the terminal
#   - it does not allow to pass arguments to Tasty to say to treat Hspec pending test as success

cabal new-run envstatus-test -- --treat-pending-as success
