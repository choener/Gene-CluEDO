#!/usr/bin/env bash

# This script runs the four data sets included with the paper "Expansion of
# Gene Clusters, Circular Orders, and the Shortest Hamiltonian Path Problem",
# Prohaska et al, 2017.

# Run this script from the main directory. It should have the binary as
# ./GeneCluEDO and the data folder under ./data.

# find GeneCluEDO binary, complain if missing.

if [ -e data ]
then
  cd data
fi

BIN=""

# local binary?
if [ -x ../GeneCluEDO ]
then
  BIN="../GeneCluEDO"
fi

# cabal new-build result
if [ -f ../Gene-CluEDO.cabal ]
then
  BIN="cabal new-run GeneCluEDO --"
fi

# binary in path?
WBIN=$(which GeneCluEDO 2>/dev/null)
if [ -x "$WBIN" ]
then
  BIN="$WBIN"
fi

if [ "$BIN" == "" ]
then
  echo "GeneCluEDO not found! Are you running from the correct directory? (The one containing the binary?)"
  echo "$BIN"
  exit 1
fi

# parameters for each file

# TODO check that these are the parameters used for the paper. (If not, they
# are somewhat sane defaults, however)

# branchiostoma lanceolatum, hox, noisy cleaned up
echo "computing: bla, hox"
$BIN --temperature 0.0025 bla-hox-noisy.dis

# homo sapiens, adh, noisy cleaned up
echo "computing: hsa, adh"
$BIN --temperature 0.0025 hsa-adh-noisy.dis

# homo sapiens, psg
echo "computing: hsa, psg"
$BIN --temperature 0.0025 hsa-psg.dis

# mus musculus, α rhox, noisy cleaned up
echo "computing: mmu rhox (takes longer)"
$BIN --temperature 0.0025 mmu-rhox-noisy.dis

