# test-accessors.R - DESC
# /test-accessors.R

# Copyright Iago MOSQUEIRA (WMR), 2019
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the GPL 3.0


# CONTEXT
context("AAP class accessors")

data(sol4)

control <- AAP.control(pGrp=TRUE, qplat.surveys=7, qplat.Fmatrix=8,
  Fage.knots=6, Ftime.knots=22, Wtime.knots=5, mcmc=FALSE)

run <- aap(sol4, sol4indices, control=control)

# CHECK accessors (but not call)
slo <- slotNames(run)

ext <- lapply(slo[-match("call", slo)], function(x) {
  do.call(x, list(run))})
ext$Class <- "AAP"

out <- do.call("new", res)

validObject(out)
