## documentation needed

# use pipe without loading package
`%>%` <- magrittr::`%>%`

#' Compute Share of Outside Good
#'
#' @param df Dataframe to work with
#' @param mkt_share Variable containing product market shares, as a character string.
#' @param market_id List of variables contaning market identifiers
#' @return A dataframe with the variable \code{outside_share} added as a column.
#'
#' @export
#'
#' @examples
#' # Add Outside Share column to data
#' mkts <- create_markets(geog_id = "market_id", time_id = "year")
#' df   <- mkt_share_from_sales(eurocars,
#'                                quantity   = 'qty_sold',
#'                                population = 'population')
#' df2  <- compute_outside_share(df, mkt_share = 'mkt_share', market_id = mkts)
compute_outside_share <- function(df, mkt_share, market_id) {
    # unpack market_ids to a vector so can group by easily
    markets <- unlist(market_id)
    # generate outside share
    outside_share <- df %>%
            dplyr::group_by(!!! rlang::syms(markets)) %>%
            dplyr::mutate(inside_share = sum(!!! rlang::sym(mkt_share))) %>%
            dplyr::mutate(outside_share = 1 - inside_share) %>%
            # ungroup the data and clean up columns we don't want returned
            dplyr::ungroup() %>%
            dplyr::select(-inside_share)
            ## TODO: add asserts that share are between zero and one

    return(outside_share)
}

#' Calculate Within Shares for First Layer Nest
#'
#' @param df Dataframe to work with
#' @param mkt_share Variable containing product market shares, as a character string.
#' @param nest_id Variable containing market share of nest, as a character string.
#' @param subnest_id Variable containing market share of subnest, as a character string.
#' @return A dataframe with the outside- and nest-shares added..
gen_nest_share <- function(df, mkt_share, market_id, nest_id){

    # unpack market_ids to a vector so can group by easily
    markets <- unlist(market_id)

    mkt_and_nests <- c(markets, nest_id)

    nest_share <- df %>%
            dplyr::group_by(!!! rlang::syms(mkt_and_nests)) %>%
            dplyr::mutate(nest_share = sum(!!! rlang::sym(mkt_share))) %>%
            dplyr::ungroup()
}

#' Calculate Within Shares for Subnest Structure
#'
#' @param df Dataframe to work with
#' @param mkt_share Variable containing product market shares, as a character string.
#' @param nest_id Variable containing market share of nest, as a character string.
#' @param subnest_id Variable containing market share of subnest, as a character string.
#' @return A dataframe with the outside- and nest-shares added.
gen_subnest_share <- function(df, mkt_share, market_id, nest_id, subnest_id){

    # unpack market_ids to a vector so can group by easily
    markets <- unlist(market_id)

    mkt_and_nests <- c(markets, nest_id, subnest_id)

    nest_share <- df %>%
            dplyr::group_by(!!! rlang::syms(mkt_and_nests)) %>%
            dplyr::mutate(subnest_share = sum(!!! rlang::sym(mkt_share))) %>%
            dplyr::ungroup()
}

#' Calculate Within Shares for Nesting Structure
#'
#' @param df Dataframe to work with
#' @param mkt_share Variable containing product market shares, as a character string.
#' @param nest_share Variable containing market share of nest, as a character string.
#' @param subnest_share Variable containing market share of subnest, as a character string.
#' @return A dataframe with the outside- and nest-shares added.
gen_within_share <- function(df, mkt_share, nest_share, subnest_share = NULL){
    # if one level of nesting
    if(is.null(subnest_share)){
        df <- within_share_one_nest(df, mkt_share, nest_share = nest_share)
        return(df)
    }

    # if two levels of nesting
    if(!is.null(subnest_share)){
        df <- within_share_two_nest(df, mkt_share,
                                nest_share = nest_share,
                                subnest_share = subnest_share)
        return(df)
    }
} # eof

#' Calculate Product level Within Shares for Two Layer Nest
#'
#' @param df Dataframe to work with
#' @param mkt_share Variable containing product market shares, as a character string.
#' @param nest_share Variable containing market share of nest, as a character string.
#' @param subnest_share Variable containing market share of subnest, as a character string.
#' @return A dataframe with the outside- and nest-shares added.
within_share_two_nest <- function(df, mkt_share, nest_share, subnest_share){
    output <- df %>%
                dplyr::mutate(
                       within_subnest = (!!rlang::sym(mkt_share)) /
                                            (!!rlang::sym(subnest_share)),
                       within_nest    = (!!rlang::sym(subnest_share)) /
                                            (!!rlang::sym(nest_share))
                    )
    return(output)
}

#' Calculate Product level Within Shares for One Layer Nest
#'
#' @param df Dataframe to work with
#' @param mkt_share Variable containing product market shares, as a character string.
#' @param nest_share Variable containing market share of nest, as a character string.
#' @return A dataframe with the outside- and nest-shares added.
within_share_one_nest <- function(df, mkt_share, nest_share){

    output <- df %>%
                dplyr::mutate(
                       within_nest = (!!rlang::sym(mkt_share)) /
                                            (!!rlang::sym(nest_share))
                    )
    return(output)
}

#' Compute Share of Outside Good and Nest Shares
#'
#' @param df Dataframe to work with
#' @param market_id List of variables contaning market identifiers
#' @param mkt_share Variable containing product market shares, as a character string.
#' @param outside_share Variable outside shares, as a character string.
#' @param nest_id Variable contaning first level nest, as a character string.
#' @param subnest_id Variable contaning second level nest, as a character string.
#' @return A dataframe with the outside- and nest-shares added.
#'
#' @export
#'
#' @examples
#' # Add Outside Share and Nest Share for two Level Nest
#' mkts <- create_markets(geog_id = "market_id", time_id = "year")
#' df   <- mkt_share_from_sales(eurocars,
#'                                quantity   = 'qty_sold',
#'                                population = 'population')
#' df2  <- create_shares(df, market_id  = mkts,
#'                           mkt_share  = 'mkt_share',
#'                           nest_id    = 'class_id',
#'                           subnest_id = 'domestic')
#' # Add Outside Share and Nest Share for One Level Nest
#' mkts <- create_markets(geog_id = "market_id", time_id = "year")
#' df   <- mkt_share_from_sales(eurocars,
#'                                quantity   = 'qty_sold',
#'                                population = 'population')
#' df2  <- create_shares(df, market_id  = mkts,
#'                           mkt_share  = 'mkt_share',
#'                           nest_id    = 'class_id')
#' # Add Nest Share for two Level Nest when outside share already computed
#' mkts <- create_markets(geog_id = "market_id", time_id = "year")
#' df   <- mkt_share_from_sales(eurocars,
#'                                quantity   = 'qty_sold',
#'                                population = 'population')
#' df2  <- compute_outside_share(df, mkt_share = 'mkt_share', market_id = mkts)
#' df3  <- create_shares(df2, market_id    = mkts,
#'                            mkt_share     = 'mkt_share',
#'                            outside_share = 'outside_share',
#'                            nest_id       = 'class_id')
create_shares <- function(df, market_id, mkt_share, outside_share = NULL,
                         nest_id = NULL, subnest_id = NULL){

    # generate outside share if needed
    if(is.null(outside_share)){
        print("Creating Outside Share...")
        df <- df %>%
                compute_outside_share(., mkt_share, market_id)
        print("Done!")
    } # end if
    # generate nest share for one level nest
    if(!is.null(nest_id) && is.null(subnest_id)){
        print("Working with one layer of nests...")
        print(paste("Nest ID is:", nest_id))
        df <- df %>%
                gen_nest_share(., mkt_share, market_id, nest_id) %>%
                gen_within_share(., mkt_share, nest_share = "nest_share",
                                 subnest_share = NULL) %>%
                dplyr::select(-nest_share)
    } #end if

    if(!is.null(nest_id) && !is.null(subnest_id)){
        print("Working with two layers of nests...")
        print(paste("Nest ID is:", nest_id))
        print(paste("Sub-Nest ID is:", subnest_id))
        df <- df %>%
                gen_nest_share(., mkt_share, market_id, nest_id) %>%
                gen_subnest_share(., mkt_share, market_id,
                                    nest_id, subnest_id) %>%
                gen_within_share(., mkt_share, nest_share = "nest_share",
                                subnest_share = "subnest_share") %>%
                dplyr::select(-nest_share, -subnest_share)

    } # endif
    return(df)
}

#' Create Market Shares from Quantity Sold
#'
#' @param df Dataframe to work with
#' @param quantity Variable containing number of goods sold, as a character string.
#' @param population Variable containing number of number of individuals
#'        in the market, as a string.
#' @param frac_pop Fraction of population to use when obtaining
#'        market shares, between 0 and 1.
#' @return A dataframe with the variable \code{mkt_share} added as a column.
#'
#' @export
#'
#' @examples
#' # Relevant market is entire population
#' new_df <- mkt_share_from_sales(eurocars,
#'                                quantity   = 'qty_sold',
#'                                population = 'population')
#' # Relevant market is 1/2 the entire population
#' new_df <- mkt_share_from_sales(eurocars,
#'                                quantity   = 'qty_sold',
#'                                population = 'population',
#'                                frac_pop   =  0.5)
mkt_share_from_sales <- function(df, quantity, population, frac_pop = 1){
    output <- df %>%
        dplyr::mutate(
            mkt_share = !!rlang::sym(quantity) /
                            (frac_pop * !!rlang::sym(population))
        )
    return(output)
}
