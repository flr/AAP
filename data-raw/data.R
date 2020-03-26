# data.R - DESC
# /data.R

# Copyright Iago MOSQUEIRA (WMR), 2019
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the GPL 3.0


library(AAP)

sol4 <- readFLStock("sol4/index.txt", na.strings="-1")

# COMPLETE slots
discards(sol4) <- computeDiscards(sol4)
catch(sol4) <- computeCatch(sol4, "all")

# SET units & range
units(sol4) <- standardUnits(sol4, biomass="tonnes",
  numbers="thousands")
range(sol4, c("minfbar", "maxfbar", "plusgroup")) <- c(2, 6, 15)

# SET plusgroup
sol4 <- setPlusGroup(sol4, 10)

# --- INDEX

# INDEX frpm VPA file
indices <- readFLIndices("sol4/fleet.txt", na.strings="-1")

# SUBSET indices
indices <- indices[c("BTS", "SNS")]

# --- SA

# RUN

control <- AAP.control(pGrp=TRUE, qplat.surveys=8, qplat.Fmatrix=9,
  Sage.knots=6, Fage.knots=8, Ftime.knots=28, Wtime.knots=5, mcmc=FALSE)

fit <- aap(sol4, indices, control=control, model="sole")

# --- SAVE

# SAVE objects
save(sol4, indices, fit, file="../data/sol4.RData", compress="xz")
