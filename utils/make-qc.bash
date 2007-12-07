#!/bin/bash
# from the main grammar directory

# back up quick check file
mv pet/qc.tdl pet/qc.tdl.old

# flop the grammar once
flop japanese 

# calculate the quickcheck file
cut -d@ -f7 tsdb/skeletons/kinou1/item | \
iconv -f utf-8 -t euc-jp  | chasen -F "%M "| iconv -f euc-jp -t utf-8 | \
cheap -limit=50000 -packing -compute-qc=pet/qc.tdl japanese

# flop the grammar again
flop japanese