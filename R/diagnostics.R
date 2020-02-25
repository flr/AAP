# diagnostics.R - DESC
# /diagnostics.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


control <- AAP.control(pGrp=TRUE, qplat.surveys=8, qplat.Fmatrix=9,
  Sage.knots=6, Fage.knots=8, Ftime.knots=28, Wtime.knots=5, mcmc=FALSE)

run <- aap(stock, indices[c("BTS-ISIS", "SNS")], control=control)

inp <- landings.n(stock)

out <- parallel::mclapply(seq(length(inp)), function(i) {
  tmp <- stock
  ln <- c(landings.n(tmp))
  ln[i] <- NA
  landings.n(tmp)[] <- ln

  irun <- aap(tmp, indices[c("BTS-ISIS", "SNS")], control=control,
    stdfile=run@stdfile)
  return(c(landings.n(run))[i])
}, mc.cores=4)


hat <- landings.n(stock)
hat[] <- unlist(out)

loor <- residuals(landings.n(stock), hat)

# PLOT

ggplot(loor, aes(x=year, y=as.factor(age))) +
  geom_point(aes(size=abs(data), fill=as.factor(sign(data))),
    shape=21, alpha=0.4, na.rm=TRUE) +
  scale_size(range = c(0.1, 8)) +
  theme(legend.position="none") + xlab("") + ylab("")

# How many positive/negative?
sum(loor>=0, na.rm=TRUE) / length(loor)
sum(loor<0, na.rm=TRUE) / length(loor)

# Value positive/negative
sum(loor[loor > 0], na.rm=TRUE)
sum(loor[loor < 0], na.rm=TRUE)

# Distribution values positive/negative

