#' eurocars: Annual Sales of Cars in Europe
#'
#'
#' Annual model-level sales of cars in European countries between 1970 and 1999.
#'
#' Data Source: Brenkers and Verboven, 2006, "Liberalizing a Distribution System: The European Car Market",
#' Journal of the European Economic Association
#'
#' @format A data.frame with 11,549 observations on 39 variables:
#' \itemize{
#'  \item \strong{year:} Year of the observation
#'  \item \strong{market_id:} Numeric identifier for geographical market
#'  \item \strong{model_id:} Numeric identifier for the car
#'  \item \strong{model_id2:} Numeric identifier for the car with unified products when model number changes
#'  \item \strong{type:} Car Type
#'  \item \strong{brand:} Brand of Manufacturer
#'  \item \strong{model:} Car Model
#'  \item \strong{origin_id:} Numeric identifier for country of origin
#'  \item \strong{location_id:} Numeric identifier for location
#'  \item \strong{class_id:} Numeric vehicle class identifier
#'  \item \strong{domestic:} == 1 if produced domestically
#'  \item \strong{firm_id:} Numeric identifier for firm that produced vehicle
#'  \item \strong{qty_sold:} Quantity of cars sold
#'  \item \strong{displacement:} Engine capacity in cc
#'  \item \strong{kilowatts:} Engine Power in kilowatts
#'  \item \strong{weight:} Vehicle weight in kilograms
#'  \item \strong{n_seats:} Number of seats
#'  \item \strong{n_doors:} Number of doors
#'  \item \strong{length:} Vehicle length in cm
#'  \item \strong{height:} Vehicle height in cm
#'  \item \strong{fueleff_90:} Fuel Efficiency at 90 km/hr
#'  \item \strong{fueleff_90:} Fuel Efficiency at 120 km/hr
#'  \item \strong{fueleff_city:} Fuel Efficiency when driving in city
#'  \item \strong{fueleff_avg:} Average Fuel Efficiency
#'  \item \strong{max_speed:} Maximum speed in km/hr
#'  \item \strong{accel:} Time taken in seconds from 0 to 100 km/hr
#'  \item \strong{price:} Price in local currency
#'  \item \strong{price_euro:} Price in Euros
#'  \item \strong{exchrate_exp:} Exchange rate in exporting country
#'  \item \strong{exchrate_imp:} Exchange rate in importing country
#'  \item \strong{cpi_exp:} Consumer Price Index in exporting country
#'  \item \strong{ppi_exp:} Producer Price Index in exporting country
#'  \item \strong{cpi_imp:} Consumer Price Index in importing country
#'  \item \strong{ppi_imp:} Producer Price Index in importing country
#'  \item \strong{tax:} Tax rate on purchase
#'  \item \strong{population:} Country of sale's population
#'  \item \strong{ngdp:} Nominal GDP in country of sale
#'  \item \strong{rgdp:} Real GDP in country of sale
#' }
#'
#' @docType data
#' @name eurocars
#' @usage data('eurocars')
#' @examples  str(eurocars)
"eurocars"
