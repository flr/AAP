# xval.R - DESC
# /xval.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# aaphcxval {{{

#' Compute a retrospective hindcast cross-validation of a4a stock and indices
#'
#' The output of `a4ahcxval` consist of a list with two elements, named 'stocks'
#' and 'indices'. The first is an object of class `FLStocks`, each a peel from
#' the restrospective run. The second element is a list of `FLIndices` object.
#' The first `FLIndices` object, named 'data', is a copy of the input 'indices'
#' argument, with the additoned `catch.n` slot, if originally missing. The next
#' element, named as the final year of the data set, contains the naive prediction
#' of the input `FLIndices`, while the remaining elements are the result of a
#' hindcast prediction of the relevant indices, those within the year range of
#' as set ny `nyears`.
#'
#' @param stock Input FLStock object.
#' @param indices Input FLIndices object.
#' @param nyears Number if years for retrospective, defaults to 5.
#' @param nsq Number of years for average biology and selectivity, defaults to 3.
#' @param fixed.ks Is the number of knots is splines with 'year' constant?
#' @param ... Any submodels and other arguments for the call to *sca*.
#'
#' @return A list containing elements 'stocks', of class *FLStocks*, and
#' 'indices', a list of *FLIndices* objects. See details for structure of this list.
#' @examples
#' data(sol4)
#' sxval <- aaphcxval(sol4 + fit, indices, control=control(fit),
#'   pin=stdfile(fit))
#' plotXval(indices, sxval$indices)

aaphcxval <- function(stock, indices, control, nyears=5, nsq=3, pin="missing") {

  fy <- dims(stock)$maxyear
  y0 <- dims(stock)$minyear

  # CHECK year knots

  # SELECT indices that fall within retro year range
  iyrs <- unlist(lapply(indices, function(x) dims(x)$maxyear)) >= (fy - nyears)

  if(!all(iyrs))
    warning(paste("FLIndices outside of year range are excluded from xval:",
      paste(names(indices)[!iyrs], collapse=", ")))

  # FILL index wt using stock.wt
  indices <- lapply(indices, function(x) {
    dmns <- dimnames(index(x))
    if(all(is.na(catch.wt(x))))
      catch.wt(x) <- stock.wt(stock)[dmns$age, dmns$year]
    return(x)
    }
  )

  # LOOP
  retro <- foreach(y=seq(fy, fy - nyears)) %dopar% {

    # RUN
    fit <- aap(window(stock, end=y), lapply(indices, window, end=y),
      control=control, pin=pin)

    # UPDATE
    stock.n(stock)[, ac(y0:y)] <- stock.n(fit) 
    harvest(stock)[, ac(y0:y)] <- harvest(fit)

    # PREDICT stock, unless y == fy
    if(y < fy) {

     # SET future SELEX, WTs
      fyrs <- ac(seq(y-nsq-1, y))

      harvest(stock)[, ac(y+1)] <- yearMeans(harvest(stock)[, fyrs])
      stock.wt(stock)[, ac(y+1)] <- yearMeans(stock.wt(stock)[, fyrs])
      catch.wt(stock)[, ac(y+1)] <- yearMeans(catch.wt(stock)[, fyrs])
      landings.wt(stock)[, ac(y+1)] <- yearMeans(landings.wt(stock)[, fyrs])
      discards.wt(stock)[, ac(y+1)] <- yearMeans(discards.wt(stock)[, fyrs])

      # REC
      srr <- predictModel(model=rec~a,
        params=FLPar(a=exp(mean(log(rec(stock)), na.rm=TRUE))))

      # FWD
      pred <- fwd(stock, control=fwdControl(year=seq(y+1, fy),
        quant="catch", value=catch(stock)[, ac(seq(y+1, fy))]), sr=srr)
    } else {
      pred <- stock
    }

    # INDEX qs
    qs <- q.hat(fit)
    
    # PREDICT FLIndices

    ihat <- mapply(function(a, b) {
      
      # GET dims
      dmns <- dimnames(b)
      dis <- dims(b)
      timf <- mean(range(b)[c("startf", "endf")])

      # COMPUTE predicted index
      index(b) <- a[dmns$age, ] %*% stock.n(pred)[dmns$age, ac(seq(dis$minyear, fy))] *
        exp(-z(pred)[dmns$age, ac(seq(dis$minyear, fy))] * timf)

      # STORE catchabilities
      index.q(b)[] <- a[dmns$age,]

      return(b)

      }, a=qs[iyrs], b=indices[iyrs], SIMPLIFY=FALSE)

    name(pred) <- paste(name(pred), y, sep="_")
    desc(pred) <- paste(".", desc(pred), "xval -", y)

    list(stock=pred, indices=ihat)
  }

  # OUTPUT: stocks (FLStocks)
  stocks <- lapply(retro, "[[", "stock")
  names(stocks) <- seq(fy, fy - nyears)

  # CONVERT to retro
  stocks <- FLStocks(lapply(names(stocks),
    function(x) window(stocks[[x]], end=x)))
  names(stocks) <- seq(fy, fy - nyears)

  # indices, first element is data, in case catch.wt had to be added
  indices <- c(list(indices), lapply(retro, function(x) FLIndices(x$indices)))
  names(indices) <- c("data", seq(fy, fy - nyears))

  list(stocks=stocks, indices=indices)
} # }}}
