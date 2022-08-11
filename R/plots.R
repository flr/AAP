# plots.R - DESC
# /plots.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# plot(AAP) {{{

setMethod("plot", signature(x="AAP", y="missing"),
  function(x) {

    dat <- metrics(x)
   
    ps <- lapply(dat, plot)
   
    ps[1:3] <- lapply(ps[1:3], "+",  theme(
      plot.margin = unit(c(0.25,0.25,0,0), units = "lines" ),
      axis.text.x = element_blank(), axis.title.x = element_blank(),
      axis.ticks.x = element_blank()))
    
    ps[[4]] <- ps[[4]] + theme(
      plot.margin = unit(c(0.25,0.25,0,0), units = "lines" ))

    ps <- mapply("+", ps, list(ylab("Recruits (thousands)"), ylab("SSB (t)"),
      ylab("Catch (t)"),
      ylab(paste0("F (", paste(unname(range(x)[c("minfbar", "maxfbar")]), collapse="-"),
        ")")) ), SIMPLIFY=FALSE)

    ps <- lapply(ps, "+", ylim(c(0,NA)))

    Reduce("/", ps)
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
  function(x, y, mrho="missing", ...) {

    x <- metrics(x)[c("Rec", "SSB", "F")] 
    y <- lapply(list(Rec=rec, SSB=ssb, F=fbar), function(i) 
      FLQuants(lapply(y, metrics, i)))

    if(missing(mrho)) {
      Reduce("/", mapply(function(x, y, z) plot(x, y) + ylab(z),
        x, y, list("Recruits (thousands)", "SSB (tonnes)", " F (2-6)"),
        SIMPLIFY=FALSE))
    } else {
      mrho <- setNames(lapply(names(mrho),
        function(x) paste0("rho(", x, ") = ", format(mrho[[x]], digits=3))),
          names(mrho))[names(x)]

      Reduce("/", mapply(function(x, y, z, a)
        plot(x, y) + ylab(z) + ggtitle(a),
        x, y,list("Recruits (thousands)", "SSB (tonnes)", "F (2-6)"),
        mrho, SIMPLIFY=FALSE))
    }
  }
) # }}}
