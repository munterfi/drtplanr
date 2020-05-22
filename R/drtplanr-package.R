#' @title Demand-responsive transport planner
#'
#' @name drtplanr-package
#' @aliases drtplanr-package
#' @docType package
#' @author Merlin Unterfinger - \email{info@@munterfinger.ch}
#' @description Tool for placing virtual stations in demand-responsive transport systems in villages by defining and minimizing a global energy (\code{drtplanr}, name is inspired by \code{stplanr} <https://github.com/ropensci/stplanr>). The station locations are randomly initialized in the street network and iteratively optimized based on the reachable population in combination with walking and driving times.
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
#' @import data.table
#' @import sf
## usethis namespace: end
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    sprintf(
      "drtplanr %s: Experimental demand-responsive transport planner\nContact: Merlin Unterfinger <info@munterfinger.ch>",
      utils::packageVersion("drtplanr")
    )
  )
}

#' Message with timestamp
#'
#' @param text character, test of the message
#'
#' @return
#' None.
#'
#' @export
#'
#' @examples
#' tmessage("Test")
tmessage <- function(text) {
  message(Sys.time(), " ", text)
}
