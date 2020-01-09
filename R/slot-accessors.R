# accessors.R - DESC
# AAP/R/accessors.R

# Copyright Iago Mosqueira (WMR), 2019
# Author: Iago Mosqueira (WMR)
#
# Distributed under the terms of the %LICENSE%


# landings.wt
setMethod("landings.wt", signature(object="AAP"),
  function(object) {
    return(slot(object, "landings.wt"))
  })

# discards.wt
setMethod("discards.wt", signature(object="AAP"),
  function(object) {
    return(slot(object, "discards.wt"))
  })

# stock.wt
setMethod("stock.wt", signature(object="AAP"),
  function(object) {
    return(slot(object, "stock.wt"))
  })

# control
setGeneric("control", function(object, ...)
    standardGeneric("control"))
setMethod("control", signature(object="AAP"),
  function(object) {
    return(slot(object, "control"))
  })
# harvest
setMethod("harvest", signature(object="AAP"),
  function(object) {
    return(slot(object, "harvest"))
  })
# index
setMethod("index", signature(object="AAP"),
  function(object) {
    return(slot(object, "index"))
  })
# index.var
setMethod("index.var", signature(object="AAP"),
  function(object) {
    return(slot(object, "index.var"))
  })
# landings.n
setMethod("landings.n", signature(object="AAP"),
  function(object) {
    return(slot(object, "landings.n"))
  })
# discards.n
setMethod("discards.n", signature(object="AAP"),
  function(object) {
    return(slot(object, "discards.n"))
  })
# q.hat
setGeneric("q.hat", function(object, ...)
    standardGeneric("q.hat"))
setMethod("q.hat", signature(object="AAP"),
  function(object) {
    return(slot(object, "q.hat"))
  })
# catch.n
setMethod("catch.n", signature(object="AAP"),
  function(object) {
    return(slot(object, "catch.n"))
  })
# index.name
setMethod("index.name", signature(object="AAP"),
  function(object) {
    return(slot(object, "index.name"))
  })
# index.res
setMethod("index.res", signature(object="AAP"),
  function(object) {
    return(slot(object, "index.res"))
  })
# landings.var
setGeneric("landings.var", function(object, ...)
    standardGeneric("landings.var"))
setMethod("landings.var", signature(object="AAP"),
  function(object) {
    return(slot(object, "landings.var"))
  })
# discards.var
setGeneric("discards.var", function(object, ...)
    standardGeneric("discards.var"))
setMethod("discards.var", signature(object="AAP"),
  function(object) {
    return(slot(object, "discards.var"))
  })
# stdfile
setGeneric("stdfile", function(object, ...)
    standardGeneric("stdfile"))
setMethod("stdfile", signature(object="AAP"),
  function(object) {
    return(slot(object, "stdfile"))
  })
# stock.n
setMethod("stock.n", signature(object="AAP"),
  function(object) {
    return(slot(object, "stock.n"))
  })
# index.range
setMethod("index.range", signature(object="AAP"),
  function(object) {
    return(slot(object, "index.range"))
  })
# index.hat
setMethod("index.hat", signature(object="AAP"),
  function(object) {
    return(slot(object, "index.hat"))
  })
