# data.R - DESC
# /data.R

# Copyright Iago MOSQUEIRA (WMR), 2019
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the GPL 3.0


library(AAP)

dat <- read.csv("sol4/sol4.csv")

sol4 <- as.FLStock(dat)

range(sol4, c("minfbar", "maxfbar")) <- c(2, 6)


# --- INDICES

# BTS

dat <- read.csv("sol4/bts.csv")

bts <- lapply(setNames(nm=unique(dat$slot)), function(x) {
  as.FLQuant(dat[dat$slot == x, -1])
})

bts <- do.call("FLIndex", bts)

name(bts) <- "BTS"
desc(bts) <- "GAM-standardized Beam Trawl Survey Q3 (NL, BE, DE)"
range(bts, c("startf", "endf")) <- c(0.66, 0.75)

# SNS

dat <- read.csv("sol4/sns.csv")

sns <- lapply(setNames(nm=unique(dat$slot)), function(x) {
  as.FLQuant(dat[dat$slot == x, -1])
})

sns <- do.call("FLIndex", sns)
name(sns) <- "SNS"
desc(sns) <- "Sole Net Survey"
range(sns, c("startf", "endf")) <- c(0.66, 0.75)

indices <- FLIndices(BTS=bts, SNS=sns)

# --- SA

# RUN

control <- AAP.control(pGrp=TRUE, qplat.surveys=8, qplat.Fmatrix=9,
  Sage.knots=6, Fage.knots=8, Ftime.knots=28, Wtime.knots=5, mcmc=FALSE)

fit <- aap(sol4, indices, control=control, model="sole")

# --- SAVE

# SAVE objects
save(sol4, indices, fit, file="../data/sol4.RData", compress="xz")
