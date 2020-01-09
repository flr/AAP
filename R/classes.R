# classes.R - DESC
# AAP/R/classes.R

# Copyright Iago Mosqueira (WMR), 2019
# Author: Iago Mosqueira (WMR)
#
# Distributed under the terms of the %LICENSE%

# FLCLASS

#' A class for 
#'
#' Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque eleifend
#' odio ac rutrum luctus. Aenean placerat porttitor commodo. Pellentesque eget porta
#' libero. Pellentesque molestie mi sed orci feugiat, non mollis enim tristique. 
#' Suspendisse eu sapien vitae arcu lobortis ultrices vitae ac velit. Curabitur id 
#' 
#' @name FLCLASS
#' @rdname FLCLASS
#' @docType class
#' @aliases FLCLASS FLCLASS-methods FLCLASS-class
#'
#' @section Slots:
#'     \describe{
#'     \item{SLOT}{Neque porro quisquam est qui dolorem ipsum (\code{SLOTCLASS}).}
#' }
#'
#' @section Validity:
#'
#'   \describe{
#'     \item{VALIDITY}{Neque porro quisquam est qui dolorem ipsum.}
#' }
#'
#' You can inspect the class validity function by using
#'    \code{getValidity(getClassDef('FLCatch'))}
#'
#' @section Accessors:
#' All slots in the class have accessor and replacement methods defined that
#' allow retrieving and substituting individual slots.
#'
#' The values passed for replacement need to be of the class of that slot.
#' A numeric vector can also be used when replacing FLQuant slots, and the
#' vector will be used to substitute the values in the slot, but not its other
#' attributes.
#'
#' @section Constructor:
#' A construction method exists for this class that can take named arguments for
#' any of its slots. All slots are then created to match the requirements of the
#' class validity. If an unnamed \code{FLQuant} object is provided, this is used
#' for sizing but not stored in any slot.
#'
#' @section Methods:
#' Methods exist for various calculations based on values stored in the class:
#'
#' \describe{
#'     \item{METHOD}{Neque porro quisquam est qui dolorem ipsum.}
#' }
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
#' @examples
#' 

setClass("AAP.control",
         representation(
           pGrp         ="logical",
           qplat.surveys="integer",
           qplat.Fmatrix="integer",
           Fage.knots   ="integer",
           Ftime.knots  ="integer",
           Wtime.knots  ="integer",
           mcmc         ="logical"),
         prototype=prototype(
           pGrp         =TRUE,
           qplat.surveys=as.integer(5),
           qplat.Fmatrix=as.integer(5),
           Fage.knots   =as.integer(5),
           Ftime.knots  =as.integer(5),
           Wtime.knots  =as.integer(7),
           mcmc         =FALSE)
         )

setClass("AAP",
         contains='FLAssess',
         representation(
           landings.wt  ="FLQuant",
           landings.n   ="FLQuant",
           landings.var ="FLQuant",
           discards.wt  ="FLQuant",
           discards.n   ="FLQuant",
           discards.var ="FLQuant",
           stock.wt     ="FLQuant",
           q.hat        ="FLQuants",
           stdfile      ="data.frame",
           control      ="AAP.control"),
         prototype=prototype(
           landings.wt  =FLQuant(),
           landings.n   =FLQuant(),
           landings.var =FLQuant(),
           discards.wt  =FLQuant(),
           discards.n   =FLQuant(),
           discards.var =FLQuant(),
           stock.wt     =FLQuant(),  
           q.hat        =FLQuants(),
           stdfile      =data.frame(),
           control      =new("AAP.control"))
)
