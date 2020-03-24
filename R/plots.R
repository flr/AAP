# plots.R - DESC
# /plots.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# plot(AAP) {{{

setMethod("plot", signature(x="AAP", y="missing"),
  function(x) {
    plot(metrics(x))
  }
)

setMethod("plot", signature(x="AAP", y="FLPar"),
  function(x, y, facet=c(SSB="B", F="F")) {

    # APPLY map to get facet
    idx <- lapply(facet, grep, dimnames(y)$params)

    # CONSTRUCT facetted FLPar data.frame
    dat <- cbind(as.data.frame(y), qname="", stringsAsFactors=FALSE)
    # BUG
    dat$params <- as.character(dat$params)
    dat[idx[[1]],"qname"] <- names(idx)[1]
    dat[idx[[2]],"qname"] <- names(idx)[2]

    # MERGE refpts with same value
    counts <- table(dat$data)
    dups <- counts[counts > 1]
    idx <- which(dat$data == names(dups))
    dat[idx[1], "params"] <- paste(as.character(dat[idx, "params"]),
      collapse=" - ")

    # REMOVE merged rows
    dat <- dat[-idx[-1],]

    # GET metrics with FLPar facetted
    mets <- metrics(x)[names(facet)]

    # PLOT
    plot(mets) +
      geom_hline(data=dat, aes(yintercept=data, colour=params)) +
      geom_text(data=dat, aes(x=dims(x)$minyear, y=data - (data * 0.10),
        label=params, colour=params), hjust=0)
  }
)
# }}}

# plot(AAP, FLStocks) {{{

setMethod("plot", signature(x="AAP", y="FLStocks"),
  function(x, y, ...) {

    x <- metrics(x)[c("Rec", "SSB", "F")] 
    y <- lapply(list(Rec=rec, SSB=ssb, F=fbar), function(i) 
      FLQuants(lapply(y, metrics, i)))

    Reduce("/", mapply(function(x, y, z) plot(x, y) + ylab(z),
      x, y, list("Recruits (thousands)", "SSB (tonnes)", " F (2-6)"),
      SIMPLIFY=FALSE))
  }
) # }}}
