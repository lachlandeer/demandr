#' Add together two numbers.
#'
#' @param geog_id Variable that represents geographical market identifier as a string.
#' @param time_id Variable that represents time identifier as a string.
#' @return A list containing \code{geog_id} and \code{time_id}.
#'
#' @examples
#' # cross section data
#' create_markets(geog_id = "state", time_id = NULL)
#' # time series data
#' create_markets(geog_id = NULL, time_id = "year")
#' # panel data
#' create_markets(geog_id = "state", time_id = "year")

create_markets <- function(geog_id, time_id){
    markets <- list()
    markets[["geog_id"]] <- geog_id
    markets[["time_id"]] <- time_id
    return(markets)
}
