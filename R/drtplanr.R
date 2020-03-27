#' @title Demand-responsive transport planner
#'
#' @name drtplanr-package
#' @aliases drtplanr-package
#' @docType package
#' @author Merlin Unterfinger - \email{info@@munterfinger.ch}
#' @keywords package
#' @description Lorem ipsum
#'
#' @import data.table
#' @import sf
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    sprintf(
      "drtplanr %s: Demand-responsive transport planner\nContact: info@munterfinger.ch",
      utils::packageVersion("mrouter")
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
