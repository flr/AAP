# data.R - DESC
# /data.R

# Copyright Iago MOSQUEIRA (WMR), 2019
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the GPL 3.0


library(FLCore)

sol4 <- readFLStock("sol4/index.txt", na.strings="-1")

# COMPLETE slots
discards(sol4) <- computeDiscards(sol4)
catch(sol4) <- computeCatch(sol4, "all")

# SET units & range
units(sol4) <- standardUnits(sol4, biomass="tonnes",
  numbers="thousands")
range(sol4, c("minfbar", "maxfbar", "plusgroup")) <- c(2, 6, 15)

# SET plusgroup
stock <- setPlusGroup(sol4, 10)

# --- INDEX

# INDEX frpm VPA file
indices <- readFLIndices("sol4/fleet.txt", na.strings="-1")

# SUBSET indices
ass.indices <- indices[c("BTS-ISIS", "SNS")]

# --- SAVE

# SAVE objects
save(stock, ass.indices, file="../data/sol4.RData", compress="xz")
