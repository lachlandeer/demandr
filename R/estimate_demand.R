#' Estimate a Demand Model From Aggregate Market Shares
#'
#' @param df Dataframe to containing data to be used in estimation
#' @param market_ids List of variables contaning market identifiers
#' @param product_id Variable containing product identifier
#' @param market_share Variable containing product market shares, as a character string
#' @param outside_share Variable outside shares, as a character string
#' @param exog_charac Vector of exogenous product characteristic variables, as a character vector
#' @param price Variable containing market price, as a character string
#' @param nest_shares Vector of nest share variables, as a character vector
#' @param instruments Vector of instrument names, as a character vector - NOT CURRENTLY IMPLEMENTED
#' @param productFE Logical variable, \code{TRUE} leads to estimation with product fixed effects
#' @param marketFE Character string of which market fixed effects to contain,
#'        takes value "both", "geog" or "time"
#' @param supply_side Logical variable, \code{TRUE} leads to estimation with the
#'        constraints from a Bertrand-Nash Oligopoly Model - NOT CURRENTLY IMPLEMENTED
#'
#' @return Returns an object of class "lm" when instruments = FALSE
#'
#' @export
#'
#' @examples
#' # Regression Without Nests
#' mkts <- create_markets(geog_id = "market_id", time_id = "year")
#' price_var     <- c("price_euro")
#' exog_var      <- c("kilowatts", "fueleff_avg", "width", "height", "domestic")
#' mkt_share     <- c("mkt_share")
#' outside_share <- c("outside_share")
#' ## Create Shares
#' df   <- mkt_share_from_sales(eurocars,
#'                                quantity   = 'qty_sold',
#'                                population = 'population')
#' df2  <- create_shares(df, market_id  = mkts,
#'                           mkt_share  = 'mkt_share')
#' ## Run regression
#' reg <- estimate_demand(df             = df2,
#'                         market_ids    = mkts,
#'                         market_share  = mkt_share,
#'                         outside_share = outside_share,
#'                         exog_charac   = exog_var,
#'                         price         = price_var,
#'                         marketFE      = "both")
#'
#' # Regression With One Level Nest
#' mkts <- create_markets(geog_id = "market_id", time_id = "year")
#' price_var     <- c("price_euro")
#' exog_var      <- c("kilowatts", "fueleff_avg", "width", "height", "domestic")
#' mkt_share     <- c("mkt_share")
#' outside_share <- c("outside_share")
#' ## Create Shares
#' df   <- mkt_share_from_sales(eurocars,
#'                                quantity   = 'qty_sold',
#'                                population = 'population')
#' df2  <- create_shares(df, market_id  = mkts,
#'                           mkt_share  = 'mkt_share',
#'                           nest_id    = 'class_id')
#' ## Run regression
#' reg <- estimate_demand(df             = df2,
#'                         market_ids    = mkts,
#'                         market_share  = mkt_share,
#'                         outside_share = outside_share,
#'                         exog_charac   = exog_var,
#'                         price         = price_var,
#'                         nest_shares   = c("within_nest"),
#'                         marketFE      = "both")
#'
#' # Regression With Two Level Nest
#' mkts <- create_markets(geog_id = "market_id", time_id = "year")
#' price_var     <- c("price_euro")
#' exog_var      <- c("kilowatts", "fueleff_avg", "width", "height", "domestic")
#' mkt_share     <- c("mkt_share")
#' outside_share <- c("outside_share")
#' ## Create Shares
#' df   <- mkt_share_from_sales(eurocars,
#'                                quantity   = 'qty_sold',
#'                                population = 'population')
#' df2  <- create_shares(df, market_id  = mkts,
#'                           mkt_share  = 'mkt_share',
#'                           nest_id    = 'class_id',
#'                           subnest_id = 'domestic')
#' ## Run regression
#' reg <- estimate_demand(df             = df2,
#'                         market_ids    = mkts,
#'                         market_share  = mkt_share,
#'                         outside_share = outside_share,
#'                         exog_charac   = exog_var,
#'                         price         = price_var,
#'                         nest_shares   = c("within_nest", "within_subnest"),
#'                         marketFE      = "both")
estimate_demand <- function(df,
                            market_ids,
                            product_id,
                            market_share,
                            outside_share,
                            exog_charac,
                            price,
                            nest_shares = NULL,
                            instruments = NULL,
                            productFE = TRUE,
                            marketFE = "both",
                            supply_side = FALSE
                            ){
    # estimate OLS Model with no supply side
    if(supply_side == FALSE && is.null(instruments)){
        estimating_equation <- create_equation(market_ids,
                                                product_id,
                                                market_share,
                                                outside_share,
                                                exog_charac,
                                                price,
                                                nest_shares,
                                                marketFE,
                                                productFE,
                                                instruments)

        output <- lm(estimating_equation, data = df)
        return(output)
    }
}

#' Create Left Hand Side of Regression Equation
#'
#' @param exog_charac Vector of exogenous product characteristic variables, as a character vector
#' @param price Variable containing market price, as a character string
#' @param nest_shares Vector of nest share variables, as a character vector
#'
#' @return A character string with a partial formula of product characteristics and nest shares.
create_lhs <- function(exog_charac, price, nest_shares){

    # get log of nest shares if is not null
    if (!is.null(nest_shares)){
        #nest_shares <- unlist(nest_shares)
        log_nest_shares <- lapply("log(", paste, unlist(nest_shares), ")",
                        sep = "")[[1]]
        lhs_variables <- c(exog_charac, price, log_nest_shares)
    }
    if (is.null(nest_shares)){
        lhs_variables <- c(exog_charac, price)
    }

    lhs_formula   <- paste(lhs_variables, collapse = "+")
    return(lhs_formula)
}

#' Create Partial Formula for Fixed Effects
#'
#' @param market_ids List of variables contaning market identifiers
#' @param market_fe Character string of which market fixed effects to contain,
#'
#' @return A character string with a partial formula for market fixed effetcs.
create_fe <- function(market_ids, product_id,
                        market_fe = "both", product_fe = TRUE){

    # Create FE for Market Variables
    if(market_fe == "both"){
        print('Estimation With Geographic and Time Fixed Effects')
        #mkt_fe <- unlist(market_ids)
        mkt_fe <- lapply("as.factor(", paste, unlist(market_ids), ")",
                        sep = "")[[1]]
        mkt_fe <- paste(mkt_fe, collapse = "+")
        #return(mkt_fe)
    } else if(market_fe == "geog") {
        print('Estimation With Only Geographic Fixed Effects')
        mkt_fe <- lapply("as.factor(", paste, unlist(market_ids$geog_id), ")",
                        sep = "")[[1]]
        #return(mkt_fe)
    } else if (market_fe == "time"){
        print('Estimation With Only Time Fixed Effects')
        mkt_fe <- lapply("as.factor(", paste, unlist(market_ids$time_id), ")",
                        sep = "")[[1]]
        #return(mkt_fe)
    }

    # Create FE for Product Variables & bind to Market FE
    if(product_fe == TRUE){
        print('Estimation With Product Fixed Effects')
        prod_fe <- lapply("as.factor(", paste, product_id, ")",
                        sep = "")[[1]]
        prod_fe <-  paste(prod_fe, collapse = "+")
        # combine
        fixed_effects <- c(mkt_fe, prod_fe)
        fe_formula   <- paste(fixed_effects, collapse = "+")

        return(fe_formula)
    }else if(product_fe == FALSE){
        print('Estimation Without Product Fixed Effects')
        fixed_effects <- mkt_fe
        fe_formula    <- fixed_effects

        return(fe_formula)
    }
    # end
}

#' Create Regression Equation for Demand Estimation
#'
#' @param mkt_share Variable containing product market shares, as a character string
#' @param out_share Variable containing outside market shares, as a character string
#'
#' @return A character string with name of dependent variable.
create_rhs <- function(mkt_share, out_share){
    log_mktshare <- paste0("log(", mkt_share, ")")
    log_outshare <- paste0("log(", out_share, ")")

    dep_var <- paste(log_mktshare, log_outshare, sep = "-")
    return(dep_var)
}

#' Create Regression Equation for Demand Estimation
#'
#' @param market_ids List of variables contaning market identifiers
#' @param market_share Variable containing product market shares, as a character string
#' @param outside_share Variable containing outside market shares, as a character string
#' @param exog_charac Vector of exogenous product characteristic variables, as a character vector
#' @param price Variable containing market price, as a character string
#' @param nest_shares Vector of nest share variables, as a character vector
#' @param instruments Vector of instrument names, as a character vector
#' @param marketFE Character string of which market fixed effects to contain,
#'        takes value "both", "geog" or "time"
#'
#' @return A regression formula.
create_equation <- function(market_ids,
                            product_id,
                            market_share,
                            outside_share,
                            exog_charac,
                            price,
                            nest_shares,
                            marketFE,
                            productFE,
                            instruments = NULL
                            ){


     y             <- create_rhs(market_share, outside_share)
     lhs_charac    <- create_lhs(exog_charac, price, nest_shares)
     fixed_effects <- create_fe(market_ids, product_id,
                                market_fe = marketFE,
                                product_fe = productFE)

    if (is.null(instruments)){
        est_eq <- paste(y, lhs_charac, sep = "~")
        #print(est_eq)
        est_eq_fe <- as.formula(paste(est_eq, fixed_effects, sep = "+"))
        #print(est_eq_fe)
    }

    return(est_eq_fe)
}
