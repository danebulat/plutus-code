module Utils where 

import Ledger

-- See a validator script's PlutusCore
printScript v = unScript script
  where script = getValidator v

-- Get a validator script's address
getSrcAddress :: Ledger.ValidatorHash -> Ledger.Address
getSrcAddress = scriptHashAddress
