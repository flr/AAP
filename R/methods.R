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
makeDAT <- function(stock, numYr, qplat_Fmatrix, qplat_surveys, S_age_knots,
  F_age_knots, F_time_knots, W_time_knots, numAges, pGrp, indMPs, selSpline, X,
  WSpline, tquants) {

  cat("#############\n")

  cat("# ",name(stock),"\n# Created:",format(Sys.time(), "%d%b%Y_%Hh%M"),"\n")

  cat("# years:",range(stock)["minyear"],"-",range(stock)["maxyear"]," ; ages:",range(stock)["min"],"-",range(stock)["max"],"; q plateau Fmatrix; q plateau surveys ; Fbar range", range(stock)["minfbar"],"-", range(stock)["maxfbar"], "; number of knots in time spline \n")

  cat(numYr, numAges, qplat_Fmatrix, qplat_surveys, range(stock)["minfbar"],range(stock)["maxfbar"], S_age_knots, F_age_knots, F_time_knots, W_time_knots,
  as.integer(pGrp),"\n")

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
  
  cat("#############\n# Selectivity spline (surveys): S_age_knot knots, qplat_surveys ages (last ages are equal)","\n")
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

# AAP + FLStock: UPDATE all

setMethod("+", signature(e1="AAP", e2="FLStock"),
    function(e1, e2) {

      # UPDATE harvest, stock.n & stock.wt
      harvest(e2) <- harvest(e1)
      stock.n(e2) <- stock.n(e1)
      stock.wt(e2) <- stock.wt(e1)

      stock(e2) <- computeStock(e2)

      # UPDATE catch, landings and discards
      landings.n(e2) <- landings.n(e1)
      landings.wt(e2) <- landings.wt(e1)
      landings(e2) <- computeLandings(e2)

      discards.n(e2) <- discards.n(e1)
      discards(e2) <- computeDiscards(e2)

      catch.n(e2) <- catch.n(e1)
      catch(e2) <- computeCatch(e2, 'all')


      return(e2)
    }
)

setMethod("+", signature(e1="FLStock", e2="AAP"),
    function(e1, e2) {

      # UPDATE harvest & stock.n
      harvest(e1) <- harvest(e2)
      
      stock.n(e1) <- stock.n(e2)
      stock(e1) <- computeStock(e1)

      return(e1)
    }
)

# }}}

# residuals {{{
setMethod(residuals, signature(object="AAP"),
  function(object, stock=missing, type="logstandard") {
    
    # SURVEY(s)
    res <- object@index.res

    if(!missing(stock))
      res <- FLQuants(c(res, FLQuants(
        landings.n=residuals(landings.n(stock), landings.n(object),
          type=type, sdlog=landings.var(object)),
        discards.n=residuals(discards.n(stock), discards.n(object),
          sdlog=discards.var(object), type=type))))
    else
      res <- object@index.res
    return(res)
  }
) # }}}

# {{{ plot

setMethod("plot", signature(x="AAP", y="missing"),
  function(x) {

    # GET catch
    catches <- metrics(x, list(Catch=catch, Landings=landings,
      Discards=discards))

    # MERGE into single FLQuant
    metsC <- expand(catches[[1]], unit=names(catches))
    metsC[,,"Landings"] <- catches[["Landings"]]
    metsC[,,"Discards"] <- catches[["Discards"]]

    # GET B + F
    mets <- metrics(x, list(
      Biomass=function(y) quantSums(stock.n(y) * stock.wt(y)),
      F=function(y) quantMeans(harvest(y)[
        ac(seq(range(y)['minfbar'], range(y)['maxfbar'])),])))
    dimnames(mets[["Biomass"]]) <- list(unit="Catch")
    dimnames(mets[["F"]]) <- list(unit="Catch")

    # COMBINE metrics
    mets[["Catch"]] <- metsC

    # ADD age range to F units
    units(mets$F) <- paste(range(x)[c("minfbar", "maxfbar")], collapse="-")

    # PLOT as 3 panels, all catch FLQuants in last
    plot(mets) + ylim(c(0,NA)) + 
      theme(legend.title=element_blank(), legend.position=c(0.25,0.3),
        legend.key.height = unit(0.1, "cm"), legend.key.width = unit(0.5, "cm"),
        legend.direction="horizontal") 
  }
) # }}}

# catch, landings, discards {{{

setMethod("catch", signature(object="AAP"), function(object) {
  return(quantSums(discards(object) + landings(object)))
})

setMethod("landings", signature(object="AAP"), function(object) {
  return(quantSums(landings.n(object) * landings.wt(object)))
})

setMethod("discards", signature(object="AAP"), function(object) {
  return(quantSums(discards.n(object) * discards.wt(object)))
}) # }}}

# stdfile2pin {{{
stdfile2pin <- function(x) {

  # SUBSET
  x <- subset(x, !name %in% c("Fbar", "SSBe", "SSBo"))

  # SPLIT by name
  pin <- split(x, as.character(x$name))
  pin <- pin[match(as.character(unique(x$name)), names(pin))]

  plist <- lapply(pin, "[", , "mean")

  for(i in names(plist)) {
    cat(paste0("#", i, "\n"))
    cat(plist[[i]], "\n")
    }
} # }}}

# metricsAAP {{{

metricsAAP <- function(object) {

  yrs <- dimnames(object@stock.n)$year
  ags <- seq(1, dims(object)$max - 1)

  # DEFINE metrics
  mets <- c(Rec="log_initpop", SSB="SSBo", F="Fbar")

  # EXTRACT from stdfile
  dat <- lapply(mets, function(x) subset(object@stdfile, name == x))

  # DELETE log_initpop[1:ages-1, ], refer to initpop at year=1
  dat$Rec <- dat$Rec[ags * -1,]
  
  # CREATE df
  res <- rbindlist(lapply(dat, function(x) {
    data.frame(year=as.numeric(yrs), mean=as.numeric(x$mean),
      # NOTE SQUARE std to get var
      var=x$stddev ^ 2,
      lowq=x$mean - 2 * x$stddev, uppq=x$mean + 2 * x$stddev)
  }), idcol="qname")

  # EXPONENTIATE
  res[res$qname == "Rec", c("mean", "var", "lowq", "uppq")] <- exp(
    res[res$qname == "Rec", c("mean", "var", "lowq", "uppq")])

  # FIX issue with sd precision
  res[res$qname == "Rec", "mean"] <- c(stock.n(object)[1,])

  colnames(res)[3] <- "data"

  return(res)
} # }}}

# metrics {{{
setMethod("metrics", signature(object="AAP", metrics="missing"),
  function(object) {
    return(metrics(object, metrics=list(Rec=rec, SSB=ssb, Catch=catch, F=fbar)))
  }
) # }}}

# sigmas {{{
sigmas <- function(fit) {
  FLQuants(c(list(Landings=landings.var(fit), Discards=discards.var(fit)),
    index.var(fit)))
}

# }}}

# aap.sa {{{
aap.sa <- function(stk, idx, args, tracking, ...) {

	args0 <- list(...)
	
  args0$stock <- stk
	args0$indices <- idx
	
  if(is.null(args0$control)) stop("Missing control object")
	
	fit <- do.call('aap', args0)

	stk <- stk + fit

  track(tracking, "conv.est", ac(args$ay)) <- 1
	
  list(stk = stk, tracking = tracking)
}

# }}}

# indicesAAP {{{
indicesAAP <- function(object) {

  # Map over indices
  FLIndices(Map(function(i, j, k, l) {
    # dimnames to subset with
    dms <- dimnames(i)[1:2]
    # FLIndex: index, indes.var
    return(FLIndex(index=i, index.var=expand(j[dms[[1]],], year=dms[[2]]),
      # index.q
      index.q=expand(k[dms[[1]],], year=dms[[2]]),
      # sel.pattern
      sel.pattern=expand(((1/l) / max(1/l, na.rm=TRUE))[dms[[1]],], year=dms[[2]])))
  }, i=index(fit), j=index.var(fit), k=q.hat(fit), l=sigmas(fit)[-seq(1,2)]))

} # }}}
