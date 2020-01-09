# methods.R - DESC
# %FLPKG%/R/methods.R

# Copyright %USER%, %YEAR%
# Author: %USER% <%EMAIL%>
#
# Distributed under the terms of the %LICENSE%

# METHOD

#' A method for
#'
#' Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque eleifend
#' odio ac rutrum luctus. Aenean placerat porttitor commodo. Pellentesque eget porta
#' libero. Pellentesque molestie mi sed orci feugiat, non mollis enim tristique. 
#' Suspendisse eu sapien vitae arcu lobortis ultrices vitae ac velit. Curabitur id 
#' nunc euismod ante fringilla lobortis. Aliquam ullamcorper in diam non placerat. 
#'
#' Aliquam sagittis feugiat felis eget consequat. Praesent eleifend dolor massa, 
#' vitae faucibus justo lacinia a. Cras sed erat et magna pharetra bibendum quis in 
#' mi. Sed sodales mollis arcu, sit amet venenatis lorem fringilla vel. Vivamus vitae 
#' ipsum sem. Donec malesuada purus at libero bibendum accumsan. Donec ipsum sapien, 
#' feugiat blandit arcu in, dapibus dictum felis. 
#'
#' @param PARAM Lorem ipsum dolor sit amet
#'
#' @return RETURN Lorem ipsum dolor sit amet
#'
#' @name METHOD
#' @rdname METHOD-methods
#' @aliases METHOD METHOD-methods
#'
#' @genericMethods
#' 
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
#' @examples
#' data(sol4)

# makedat {{{
makeDAT <- function(stock, numYr, qplat_Fmatrix, qplat_surveys, F_age_knots,
  F_time_knots, W_time_knots, numAges, pGrp, indMPs, selSpline, X,
  WSpline, tquants) {

  cat("#############\n")

  cat("# ",name(stock),"\n# Created:",format(Sys.time(), "%d%b%Y_%Hh%M"),"\n")

  cat("# years:",range(stock)["minyear"],"-",range(stock)["maxyear"]," ; ages:",range(stock)["min"],"-",range(stock)["max"],"; q plateau Fmatrix; q plateau surveys ; Fbar range", range(stock)["minfbar"],"-", range(stock)["maxfbar"], "; number of knots in time spline \n")

  cat(numYr, numAges, qplat_Fmatrix, qplat_surveys, range(stock)["minfbar"],range(stock)["maxfbar"], F_age_knots, F_time_knots, W_time_knots, as.integer(pGrp),"\n")

  cat(length(indMPs), unlist(indMPs))
  
  cat("\n#############\n")
  
  for (ii in 1:length(tquants)){
    write.table(tquants[[ii]], row.names=F, col.names=F,quote=F)
    cat("\n");
  }
  
  # if landings at age available then set landings to -1 so not used
  landings(stock)[,!apply(is.na(landings.n(stock)),2,all)] <- -1
  
  # if landings at age available then set landings to -1 so not used
  landings(stock)[is.na(landings(stock))] <- -1
  
  cat("#############\n# landings \n",landings(stock),"\n")
  
  cat("#############\n# M \n", m(stock)[1,1],"\n")
  
  cat("#############\n# Maturity (females) \n")
  
  tmp <- matrix(mat(stock)[,1]@.Data,ncol=1)
  tmp[is.na(tmp)] <- round(-1,0)
  write.table(tmp, row.names=F, col.names=F,quote=F)
  
  cat("#############\n# Selectivity spline (surveys): F_age_knot knots, qplat_surveys ages (last ages are equal)","\n")
  write.table(selSpline, row.names=F, col.names=F,quote=F)
  
  # TODO WHY this?
  # cat("#############\n# Annual F spline","\n")
  # write.table(Fspline, row.names=F, col.names=F,quote=F)
  
  cat("#############\n# tensor spline design matrix","\n")
  write.table(X, row.names=F, col.names=F,quote=F)
 
  cat("#############\n# Annual W spline","\n")
  write.table(WSpline, row.names=F, col.names=F,quote=F)
  
} # }}}

# `+` {{{

setMethod("+", signature(e1="AAP", e2="FLStock"),
    function(e1, e2) {

      harvest(e2) <- harvest(e1)
      stock.n(e2) <- stock.n(e1)

      stock(e2) <- computeStock(e2)

      return(e2)
    }
)

setMethod("+", signature(e1="FLStock", e2="AAP"),
    function(e1, e2) {

      harvest(e1) <- harvest(e2)
      stock.n(e1) <- stock.n(e2)

      return(e1)
    }
)

# }}}

# residuals
# https://www.rdocumentation.org/packages/stats/versions/3.6.1/topics/influence.measures

setMethod(residuals, signature(object="AAP"),
  function(object, stock=missing) {

    # SURVEY(s)

    # index
    sobs <- index(object)
    sfit <- index.hat(object)

    res <- FLQuants(mapply(function(x, y)
      Reduce(stdlogres, intersect(x, y)), sobs, sfit))

    if(!missing(stock))
      res <- c(FLQuants(
        landings.n=stdlogres(landings.n(stock), landings.n(object)),
        discards.n=stdlogres(landings.n(stock), landings.n(object))))

    return(res)
  }
) # }}}

# {{{ plot

setMethod("plot", signature(x="AAP"),
  function(x) {

    catch <- metrics(x, list(
      L=function(x) quantSums(landings.n(x) * landings.wt(x)),
      D=function(x) quantSums(discards.n(x) * discards.wt(x))))

    C <- expand(catch$L, unit=c("L", "D"))
    C[,, "D"] <- catch$D

    mets <- metrics(x, list(
      B=function(x) quantSums(stock.n(x) * stock.wt(x)),
      F=function(x) quantMeans(harvest(x))))

    # ADD age range to F units
    units(mets$F) <- paste0(dims(x)[c("min", "max")], collapse="-")

    plot(FLQuants(c(mets, list(C=C))))
  }
) # }}}
