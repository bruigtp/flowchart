# warns when big.mark equals OutDec

    Code
      res <- update_numbers(fc, big.mark = ".")
    Condition
      Warning:
      You have set `big.mark` equal to your environment's `OutDec` ('.') - it can be confusing if your flowchart uses the same mark for both.
      i Consider an alternative decimal mark.
      > To change the decimal mark, run: `options(OutDec = "<alternative decimal mark>")`

