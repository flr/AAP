# aap.R - DESC
# /aap.R

# Copyright Iago MOSQUEIRA (WMR), 2019
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the GPL 3.0

#' @param stock
#' @param indices
#' @param control
#' @param args
#' @examples
#' data(sol4)
#' control <- AAP.control(pGrp=TRUE, qplat.surveys=7, qplat.Fmatrix=8,
#'   Sage.knots=7, Fage.knots=6, Ftime.knots=22, Wtime.knots=5, mcmc=FALSE)
#' run <- aap(sol4, indices=indices, control=control)
#' run <- aap(sol4, indices=indices, control=control, stdfile=run@stdfile)
#' mcmcrun <- aap(sol4, sol4indices, AAP.control(control, mcmc=TRUE))

aap <- function(stock, indices, control, args=" ", wkdir=tempfile(),
  stdfile="missing") {

  # CHECK inputs
  # surveys age ranges covered by stock
  if(!all(unlist(lapply(indices, function(x)
    all(dimnames(index(x))$age %in% dimnames(stock.n(stock))$age)))))
    stop("Ages in survey(s) must be covered by those in stock")
  
  #get info from control
  pGrp           <- control@pGrp
  qplat_surveys  <- control@qplat.surveys
  qplat_Fmatrix  <- control@qplat.Fmatrix  
  S_age_knots    <- control@Sage.knots
  F_age_knots    <- control@Fage.knots
  F_time_knots   <- control@Ftime.knots
  W_time_knots   <- control@Wtime.knots

  # units
  nunits <- units(catch.n(stock))
  wunits <- units(catch.wt(stock))

  # Number of years
  years <- as.numeric(range(stock)["minyear"]:range(stock)["maxyear"])
  numYr <- length(years)

  # Number of ages
  numAges <- length(1:range(stock)[["max"]])

  indexVals  <- lapply(indices, index)
  numIndices <- length(indexVals)

  # Combine sex separated indices
  # index midpoints (timing during year)
  indMPs <- list()
  for (ind in names(indices)) 
    indMPs[[ind]] <- as.numeric((range(indices[[ind]])["startf"] +
      range(indices[[ind]])["endf"])/2)
 
  # CALCULATE splines
  
  # selectivity surveys
  selSpline <- format(t(matrix(bs(1:qplat_surveys,S_age_knots,intercept=T),
    ncol=S_age_knots)),nsmall=9)
 
  #USE TENSOR spline instead
  # now make design matrix for F over ages and time, and for U1
  X  <- gam(dummy ~ te(age, year, k = c(F_age_knots,F_time_knots)),
    data = expand.grid(dummy = 1, age = 1:qplat_Fmatrix,
    year = as.numeric(1:numYr)), fit = FALSE) $ X
  
  # Annual W
  WSpline <- format(t(matrix(bs(1:numYr,df=W_time_knots,intercept=T),
    ncol=W_time_knots)), nsmall=9)
  
  # GENERATE equally sized tquants that can be written to disk
  
  quants <- mcf(c(list(landings.n(stock), discards.n(stock),
    landings.wt(stock),discards.wt(stock),stock.wt(stock)), indexVals))

  for (ii in 1:length(quants)){
    if (!(ii %in% c(3,4,5))) {
      quants[[ii]] <- quants[[ii]] + min(c(quants[[ii]])[!quants[[ii]]==0],
        na.rm=TRUE)/2
    }
    quants[[ii]][is.na(quants[[ii]])] <- round(-1,0)
  }
  
  tquants <- lapply(quants,function(x) {
    x <- matrix(x, nrow=dim(x)[1])
    t(x)
  })
  
  # CREATE output dir
  if (!missing(wkdir)) {
    # CREATE directory locally as specified by the user
      wkdir.start <- wkdir
      # IF exists, try numbered versions
      kk <- 1
      ans <- file.exists(wkdir)
      while(ans) {
        wkdir <- paste(wkdir.start,"-", kk, sep = "")
        kk <- kk + 1
        ans <- file.exists(wkdir)
      }
      cat("Model and results are stored in working directory [", wkdir,"]\n",
        sep = "")
    }
    
  dir.create(wkdir, showWarnings = FALSE)

  fname <- file.path(wkdir, "aap")

  # CREATE .dat file
  capture.output(makeDAT(stock, numYr, qplat_Fmatrix,qplat_surveys,S_age_knots,  
    F_age_knots, F_time_knots,W_time_knots, numAges, pGrp, indMPs, selSpline,
    X, WSpline, tquants), file=file.path(wkdir,"aap.dat"))

  if(!missing(stdfile))
    capture.output(stdfile2pin(stdfile), file=file.path(wkdir,"aap.pin"))
  
  # 
  dmns     <- list(year=years, age=1:numAges)
  dmnsiter <- list(age=dmns[[2]],year=dmns[[1]],iter=1:1000)
  nyears   <- length(dmns[[1]])
  res     <- new("AAP")
  
  range(res)[c("min", "max", "minyear", "maxyear", "minfbar", "maxfbar")] <-
    range(stock)[c("min", "max", "minyear", "maxyear", "minfbar", "maxfbar")]
  
  # RUN
  if (!control@mcmc) {
    if (file.exists("aap.std")) file.remove("aap.std")
    echo <- system(paste0("cd ",
      shQuote(wkdir), ";aap -nox -ind aap.dat ", args))
   
    #First see if std file exists. If not: trouble
    if (file.exists(paste0(fname, ".std"))) {
      repFull <- readLines(paste0(fname, ".rep"), n=-1)
      stdfile <- readLines(paste0(fname, ".std"))
    } else {
      stop("Hessian not positive definite?")
    }
    # even if std file exists, std estimates may be lacking, also trouble :)
    if (rev(unlist(strsplit(stdfile[2]," ")))[1] %in%
      c("1.#QNBe+000","-1.#INDe+000" )) {
      stop("Hessian not positive definite?")
    }
        
    res@stdfile <- read.table(paste0(fname, ".std"), skip=1,
      col.names= c("index","name","mean","stddev"))
    
    estN <- read.table(paste0(fname,".rep"),
      skip=which(repFull=="Estimated N"), nrow=nyears)
    estF <- read.table(paste0(fname,".rep"),
      skip=which(repFull=="Estimated F"), nrow=nyears)
    estSWT <- read.table(paste0(fname,".rep"),
      skip=which(repFull=="Estimated SWT"), nrow=nyears)
      
    res@stock.n  <- as.FLQuant(t(matrix(data.matrix(estN),
      nrow=nyears, dimnames=dmns)), units=nunits)
    res@harvest  <- as.FLQuant(t(matrix(data.matrix(estF),
      nrow=nyears, dimnames=dmns)), units="f")
    res@stock.wt <- as.FLQuant(t(matrix(data.matrix(estSWT),
      nrow=nyears, dimnames=dmns)), units=wunits)
        
  # McMC
  } else if (control@mcmc) {
    
    # MLE run if no std
    if(!file.exists(paste0(fname, ".std")))
      mle <- aap(stock=stock, indices=indices,
        control=AAP.control(control, mcmc=FALSE))

    echo <- system(paste0("cd ",
      shQuote(wkdir), ";aap -mcmc 1e5 -mcsave 1e2", args))

    echo <- system(paste0("cd ",
      shQuote(wkdir), ";aap -mceval"))

    repFull <- readLines(paste0(fname,".rep"), n=-1)
    stdfile <- readLines(paste0(fname,".std"))
    estN <- array(unlist(read.table(file.path(wkdir, "N.mcmc"))),
      dim=c(numYr,1000,numAges))
    estN <- aperm(estN,c(3,1,2))
    res@stock.n  <- as.FLQuant(c(estN), dimnames=dmnsiter, units=nunits)
    
    estF <- array(unlist(read.table(file.path(wkdir, "F.mcmc"))),
      dim=c(numYr,1000,numAges))
    estF <- aperm(estF,c(3,1,2))
    res@harvest  <- as.FLQuant(c(estF), dimnames=dmnsiter, units="f")
    
    estSWT <- array(unlist(read.table(file.path(wkdir, "swt.mcmc"))),
      dim=c(numYr,1000,numAges))
    estSWT <- aperm(estSWT,c(3,1,2))
    res@stock.wt  <- as.FLQuant(c(estSWT), dimnames=dmnsiter, units=wunits)
  } 
  
  # READ in full file and stdfile
 
  estLWT <- read.table(paste0(fname,".rep"),
    skip=which(repFull=="Estimated LWT"), nrow=nyears)
  estTSB <- read.table(paste0(fname,".rep"),
    skip=which(repFull=="Estimated TSB"), nrow=1)
  estSSB <- read.table(paste0(fname,".rep"),
    skip=which(repFull=="Estimated SSB from est wts"), nrow=1)
  SSB <- read.table(paste0(fname,".rep"),
    skip=which(repFull=="Estimated SSB from obs wts"), nrow=1)
  estSELF1 <- read.table(paste0(fname,".rep"), 
    skip=which(repFull=="log_self1"), nrow=1)
  estSELU <- read.table(paste0(fname,".rep"), 
    skip=which(repFull=="log_selU"), nrow=length(indMPs))
  estLAA <- read.table(paste0(fname,".rep"),
    skip=which(repFull=="Estimated l@a"), nrow=nyears)
  estDAA <- read.table(paste0(fname,".rep"),
    skip=which(repFull=="Estimated d@a"), nrow=nyears)
  
  for (ss in 1:length(indMPs)) {
    estSurv <- read.table(paste0(fname,".rep"),
      skip=which(repFull=="Estimated surveys")+((ss-1)*nyears), nrow=nyears)

    if (ss == 1) { 
      # TODO units of index
      res@index.hat <- FLQuants(as.FLQuant(t(matrix(data.matrix(estSurv),
        nrow=nyears, dimnames=dmns))))
    } else {
       res@index.hat[[ss]] <- as.FLQuant(t(matrix(data.matrix(estSurv),
        nrow=nyears, dimnames=dmns)))
    }  
  }
   
  estsigmaL <- read.table(paste0(fname, ".rep"), 
    skip=which(repFull=="sigmaL"), nrow=1)
  estsigmaD <- read.table(paste0(fname, ".rep"), 
    skip=which(repFull=="sigmaD"), nrow=1)
  estsigmaU <- read.table(paste0(fname, ".rep"), 
    skip=which(repFull=="sigmaU"), nrow=length(indMPs))
  
  hatdmns <-list(year="all",age=dmns$age)
  
  res@landings.n <- as.FLQuant(t(matrix(data.matrix(estLAA),
    nrow=nyears, dimnames=dmns)), units=nunits)
  res@landings.wt <- as.FLQuant(t(matrix(data.matrix(estLWT),
    nrow=nyears, dimnames=dmns)), units=wunits)
  res@landings.var <- as.FLQuant(t(matrix(data.matrix(estsigmaL),
    nrow=1, dimnames=hatdmns)), units=paste0(nunits, "^2"))
  
  res@discards.n   <- as.FLQuant(t(matrix(data.matrix(estDAA),
    nrow=nyears, dimnames=dmns)), units=nunits)
  # TODO CHECK w/JJP
  res@discards.wt <- stock@discards.wt
  res@discards.var <- as.FLQuant(t(matrix(data.matrix(estsigmaD),
    nrow=1, dimnames=hatdmns)), units=paste0(nunits, "^2"))
  
  res@catch.n <- res@discards.n + res@landings.n 
  
  for (ss in 1:length(indMPs)) {
    if (ss == 1) { 
      res@q.hat <- FLQuants(as.FLQuant(t(matrix(data.matrix(estSELU[1,]),
        nrow=1, dimnames=hatdmns)), units=""))
      suppressWarnings(res@index.res <- FLQuants(log(quants[[6]])-
        log(res@index.hat[[1]])))
      res@index.var <- FLQuants(as.FLQuant(t(matrix(data.matrix(estsigmaU[1,]),
        nrow=1, dimnames=hatdmns)))) 
    } else {
      res@q.hat[[ss]] <- as.FLQuant(t(matrix(data.matrix(estSELU[ss,]),
        nrow=1, dimnames=hatdmns)))
      suppressWarnings(res@index.res[[ss]] <- log(quants[[5+ss]])-
        log(res@index.hat[[ss]]))
      res@index.var[[ss]] <- as.FLQuant(t(matrix(data.matrix(estsigmaU[ss,]),
        nrow=1, dimnames=hatdmns)))
  }  
  #set those ages in index.var to NA that have only NA in residuals
  res@index.var[[ss]][apply(is.na(res@index.res[[ss]]),1,all)] <- NA

 }
   
  res@q.hat@names <- res@index.hat@names <- res@index.var@names <-
    res@index.res@names  <- res@index@names <- indices@names

  res@index <- FLQuants(indexVals)
  res@control <- control
  res@call <- deparse(sys.calls()[[sys.nframe()]])
  
  return(res)
}
