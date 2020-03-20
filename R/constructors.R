# constructors.R - DESC
# /constructors.R

# Copyright Iago MOSQUEIRA (WMR), 2019
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the GPL 3.0

# AAP.control {{{

#' @param x=NULL
#' @param pGrp=TRUE
#' @param qplat.surveys=5
#' @param qplat.Fmatrix=5
#' @param Sage.knots=5
#' @param Fage.knots=5
#' @param Ftime.knots=5
#' @param Wtime.knots=7
#' @param mcmc=F
#' @examples
#' AAP.control()
#' AAP.control(pGrp=FALSE)
#' AAP.control(AAP.control(), pGrp=FALSE)
#' AAP.control(AAP.control(), pGrp=FALSE, qplat.surveys=3)
#' AAP.control(pGrp=FALSE, qplat.surveys=3)

# TODO CONVERT to method(s)
AAP.control <- function(x=NULL, pGrp=TRUE, qplat.surveys=5,
  qplat.Fmatrix=5, Sage.knots=7, Fage.knots=5, Ftime.knots=5, Wtime.knots=7, mcmc=F) {

  # NEW control
  if (is.null(x)) {

    x <- new("AAP.control", 
      pGrp=as.logical(pGrp)[1], qplat.surveys=as.integer(qplat.surveys),
      qplat.Fmatrix=as.integer(qplat.Fmatrix), Sage.knots=as.integer(Sage.knots),
      Fage.knots=as.integer(Fage.knots), Ftime.knots=as.integer(Ftime.knots),
      Wtime.knots=as.integer(Wtime.knots), mcmc=as.logical(mcmc)[1])

  # UPDATE existing
  } else {
    if (!is.null(x) & !( is(x, "AAP.control")))
      sop("'x' must be an 'AAP.control' object!")
    
    if (is(x, "AAP.control"))
      # CHECK passed arguments
      for(i in names(formals(AAP.control))[-1])
        if(!do.call(missing, list(i)))
          slot(x, i) <- ifelse(is.numeric(get(i)), as.integer(get(i)), get(i))
  } 
  return(x)
} # }}}

# pin {{{

pin <- function(stock, indices, control) {

  # EXTRACT dimensions
  dms <- dims(stock)
  nyears <- dms$year
  nages <- dms$age

  nsurveys <- length(indices)

  # COMPUTE lengths
  nselcoff1 <- control@Fage.knots * control@Ftime.knots
  nselcoffU <- nsurveys * control@Sage.knots
  ntempstw <- control@Wtime.knots
  ninitpop <- nyears + nages - 1


  # CREATE pin list by element
  pin <- list(
    # logsigmaL: 3
  list(name="logsigmaL", mean=rep(0.01, 3), stddev=rep(0.01, 3)),
    # logsigmaD: 3
    list(name="logsigmaD", mean=rep(0.01, 3), stddev=rep(0.01, 3)),
    # logsigmaU: no_surveys
    list(name="logsigmaU", mean=rep(0.01, nsurveys), stddev=rep(0.01, nsurveys)),
    # logsigmaLWTS: 3
    list(name="logsigmaLWTS", mean=rep(0.01, 3), stddev=rep(0.01, 3)),
    # logsigmaSWTS: 3
    list(name="logsigmaSWTS", mean=rep(0.01, 3), stddev=rep(0.01, 3)),
    # loga0: 1 (0.1-0.9)
    list(name="loga0", mean=0.5, stddev=0.01),
    # logSWfact: 1
    list(name="logSWfact", mean=0.01, stddev=0.01),
    # log_sel_coff1: F_age_knots*F_time_knots
    list(name="log_sel_coff1", mean=rep(0.5, nselcoff1), stddev=rep(0.01, nselcoff1)),
    # log_sel_coffU: no_surveys*S_age_knots
    list(name="log_sel_coffU", mean=rep(0.5, nselcoff1), stddev=rep(0.01, nselcoff1)),
    # disc_curve: 2
    list(name="disc_curve", mean=rep(0.01, 2), stddev=rep(0.01, 2)),
    # logK: 1 (-2,3)
    list(name="logaK", mean=2, stddev=0.01),
    # log_temp_wts_Linf: W_time_knots
    list(name="log_temp_wts_Linf", mean=rep(0.5, ntempstw), stddev=rep(0.01, ntempstw)),
    # log_initpop: nyrs+nages-1
    list(name="log_initpop", mean=rep(10, ninitpop), stddev=rep(0.1, ninitpop))
  )

  # CONVERT to data.frame
  pindf <- do.call(rbind, lapply(pin, as.data.frame))

  # ADD 'index' column
  pindf <- cbind(index=seq(1, nrow(pindf)), pindf)

  return(pindf)
} # }}}

