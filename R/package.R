# package.R - DESC
# %FLPKG%/R/package.R

# Copyright %USER%, %YEAR%
# Author: %USER% <%EMAIL%>
#
# Distributed under the terms of the %LICENSE%

#' What The Package Does
#'
#' Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque eleifend
#' odio ac rutrum luctus. Aenean placerat porttitor commodo. Pellentesque eget porta
#' libero. Pellentesque molestie mi sed orci feugiat, non mollis enim tristique. 
#' Suspendisse eu sapien vitae arcu lobortis ultrices vitae ac velit. Curabitur id 
#'
#' @author
#' %USER% \email{%EMAIL%, duplicate ampersand}
#'
#' Maintainer: %USER% \email{%EMAIL%, duplicate ampersand}
#' @name %pkgname%
#' @docType package
#' @import 
#' @examples
#' \dontrun{FLPKG()}
#' @seealso See \code{vignette("%FLPKG%", package = "%FLPKG%")} for an
#' overview of the package.
NULL

os.type <- function (type = c("linux", "windows", "osx", "else")) {
  type = match.arg(type)
  if (type == "windows") {
    return(.Platform$OS.type == "windows")
  }
  else if (type == "osx") {
    return(Sys.info()["sysname"] == "Darwin")
  }
  else if (type == "linux") {
#    return((.Platform$OS.type == "unix") && !os.type("mac"))
    return(.Platform$OS.type == "unix" && Sys.info()["sysname"] != "Darwin")
  }
  else if (type == "else") {
    return(TRUE)
  }
  else {
    stop("This shouldn't happen.")
  }
}

aap.dir <- function () {
  if (os.type("linux")) {
    fnm <- system.file("bin/linux", package = "AAP")
  }
  else if (os.type("osx")) {
    fnm <- system.file("bin/osx", package = "AAP")
  }
  else if (os.type("windows")) {
    fnm <- system.file("bin/windows", package = "AAP")
  }
  else {
    stop("Unknown OS")
  }
  if (file.exists(fnm)) {
    return(fnm)
  }
  else {
    stop(paste("AAP installation error; no such file", fnm))
  }
}

.onAttach <- function(libname, pkgname) {

  # FIND PATH separator
  sep <- if (os.type("linux") | os.type("osx")) ":" else if (os.type("windows")) ";" else ","
 
  path <- paste(aap.dir(), Sys.getenv("PATH"), sep=sep)

  Sys.setenv(PATH=path)
}
