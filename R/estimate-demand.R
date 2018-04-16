## DOCUMENTATION!

# estimate_demand <- function(df,
#                             market_ids,
#                             market_share,
#                             outside_share,
#                             exog_charac,
#                             price,
#                             nest_shares,
#                             instruments = NULL,
#                             marketFE = "both"
#                             supply_side = FALSE
#                             ){
#
#
# }

create_lhs <- function(exog_charac, price, nest_shares){
    lhs_variables <- c(exog_charac, price, nest_shares)
    lhs_formula   <- paste(lhs_variables, collapse = "+")
    return(lhs_formula)
}

create_fe <- function(market_ids, market_fe = "both"){

    if(market_fe == "both"){
        mkt_fe <- unlist(market_ids)
        mkt_fe <- paste(mkt_fe, collapse = "+")
        return(mkt_fe)
    } else if(market_fe == "geog") {
        mkt_fe <- market_ids[[geog_id]]
        return(mkt_fe)
    } else if (market_fe == "time"){
        mkt_fe <- market_ids[[time_id]]
        return(mkt_fe)
    }

}

create_rhs <- function(mkt_share, out_share){
    log_mktshare <- paste0("log(", mkt_share, ")")
    log_outshare <- paste0("log(", out_share, ")")

    dep_var <- paste(log_mktshare, log_outshare, collapse = "-")
    return(dep_var)
}

create_equation <- function(market_ids,
                            market_share,
                            outside_share,
                            exog_charac,
                            price,
                            nest_shares,
                            instruments = NULL,
                            marketFE = "both"){


     y             <- create_rhs(market_share, outside_share)
     lhs_charac    <- create_lhs(exog_charac, price, nest_shares)
     fixed_effects <- create_fe(market_ids, market_fe = marketFE)

    if (is.null(instruments)){
        est_eq <- paste(dep_var, lhs_formula, sep = "~")
        est_eq_fe <- as.formula(paste(est_eq, fixed_effects, sep = "|"))
    }

    return(est_eq_fe)
}
