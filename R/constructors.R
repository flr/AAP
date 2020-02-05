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
}
