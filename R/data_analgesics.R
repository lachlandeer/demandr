#' Weekly Sales of OTC Analgesics
#'
#'
#' Weekly sales data of 11 analgesics products at the store level.
#' Data is for 73 stores over 48 weeks
#'
#' @format A data.frame with 38544 observations on 8 variables:
#' \itemize{
#'  \item \strong{store_id:} Numeric identifier for each store
#'  \item \strong{week_id:} Numeric identifier for week of data
#'  \item \strong{brand_id:} Numeric identifier for brand of analgesic
#'  \item \strong{sales:} Number of units sold
#'  \item \strong{customers:} Number of customers at store in a week
#'  \item \strong{price:} Product price
#'  \item \strong{promotion:}  = TRUE if brand is on promotion
#'  \item \strong{wholesale_price:} Price store pays for product
#' }
#'
#' @docType data
#' @name analgesics
#' @usage data('analgesics')
#' @examples  str(analgesics)
"analgesics"
