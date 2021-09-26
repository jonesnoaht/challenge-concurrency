# Concurrency Challenge

## Overview 

1. Once the gold token has been made and put in an address on the
   chain, A, the intake should collect everything inside of it in
   every several slots; B, the original address of the token should be
   put into the UTxO datum.
2. Once they are collected, they go to the batch script.
3. The intake script fills the batch address until it has the
   pre-specified number of UTxOs or until the intake script has
   exhausted the supply of UTxOs at its address. All the while it adds
   the destination address as well as the number of trinkets to the
   transaction as datum. If the number of UTxOs being put into the
   batch script exceeds the maximum allowable number of UTxOs, then
   the intake script submits this transaction and creates a new
   transaction to another batch script.
4. After the batch script has received the transaction, it decodes the
   datum, burns the trinkets, and mints gold native tokens, which are
   then sent to the addresses that sent the trinkets.
   
## Client

The client merely needs to send trinkets to the "gold" script address
provided plus the necessary fee to fund the entire process. The client
will then receive gold after the transaction has been processed.

## Testing

We use quickcheck to provide the property-based testing solution that
ensures that all patterns are accepted and end in trinkets being
burned and gold being minted and sent to the original wallets.

I may have failed to finish.

## Details

### Setup

1. Create the TrinketBox NFT to receive trinkets.
2. Start the PAB, which checks the TrinketBox for trinkets and creates batches.

### TrinketBox



## Notes to Self

Below, I have included some links and sources for reference

1. [plutus/plutus-ledger.cabal at
   f4aa043660eb8f137796f6e2d6684cf8d0418c1e · input-output-hk/plutus ·
   GitHub](https://github.com/input-output-hk/plutus/blob/f4aa043660eb8f137796f6e2d6684cf8d0418c1e/plutus-ledger/plutus-ledger.cabal)
2. [plutus/Tx.hs at edd3ce253a9080cd92b785f5a2b48f7d5b09e350 ·
   input-output-hk/plutus ·
   GitHub](https://github.com/input-output-hk/plutus/blob/edd3ce253a9080cd92b785f5a2b48f7d5b09e350/plutus-ledger-api/src/Plutus/V1/Ledger/Tx.hs)
3. [Search · address ·
   GitHub](https://github.com/input-output-hk/plutus/search?q=address)
4. [plutus/Request.hs at 68fd66a6c7f20e5ce65e0c13512111e33c18668b ·
   input-output-hk/plutus ·
   GitHub](https://github.com/input-output-hk/plutus/blob/68fd66a6c7f20e5ce65e0c13512111e33c18668b/plutus-contract/src/Plutus/Contract/Request.hs)
5. [Plutus Pioneer Program & Alonzo Testnet Notes — Plutus Pioneer
   Program Lecture Notes
   documentation](https://plutus-pioneer-program.readthedocs.io/en/latest/index.html)
6. [6. Week 06 - Oracles — Plutus Pioneer Program Lecture Notes
   documentation](https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week6.html#oracle-pab)
7. [5. Week 05 - Native Tokens — Plutus Pioneer Program Lecture Notes
   documentation](https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week5.html)
8. [1. Minting Tokens — Plutus Pioneer Program Lecture Notes
   documentation](https://plutus-pioneer-program.readthedocs.io/en/latest/alonzo/minting_tokens.html#mint-tokens)
9. [plutus/StateMachine.hs at master · input-output-hk/plutus ·
   GitHub](https://github.com/input-output-hk/plutus/blob/master/plutus-contract/src/Plutus/Contract/StateMachine.hs)
10. [7. Week 07 - State Machines — Plutus Pioneer Program Lecture
    Notes
    documentation](https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week7.html)
11. [Here](https://www.haskell.org/cabal/) is a useful guide on
    starting up cabal.
