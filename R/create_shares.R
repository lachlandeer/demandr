## documentation needed

# use pipe without loading package
`%>%` <- magrittr::`%>%`

compute_outside_share <- function(df, mkt_share, market_id) {


    # unpack market_ids to a vector so can group by easily
    markets <- unlist(market_id)

    # generate outside share
    outside_share <- df %>%
            dplyr::group_by(!!! rlang::syms(markets)) %>%
            dplyr::mutate(inside_share = sum(!!! rlang::sym(mkt_share))) %>%
            dplyr::mutate(outside_share = 1 - inside_share) %>%
            # ungroup the data and clean up columns we don't want returned
            ungroup() %>%
            dplyr::select(-inside_share)
            ## TODO: add asserts that share are between zero and one

}

## dplyr approach to nest share (testing)
gen_nest_share <- function(df, mkt_share, market_id, nest_id){

    # unpack market_ids to a vector so can group by easily
    markets <- unlist(market_id)

    mkt_and_nests <- c(markets, nest_id)

    nest_share <- df %>%
            dplyr::group_by(!!! rlang::syms(mkt_and_nests)) %>%
            dplyr::mutate(nest_share = sum(!!! rlang::sym(mkt_share))) %>%
            ungroup()
}

## dplyr approach to subnest share (testing)
gen_subnest_share <- function(df, mkt_share, market_id, nest_id, subnest_id){

    # unpack market_ids to a vector so can group by easily
    markets <- unlist(market_id)

    mkt_and_nests <- c(markets, nest_id, subnest_id)

    nest_share <- df %>%
            dplyr::group_by(!!! rlang::syms(mkt_and_nests)) %>%
            dplyr::mutate(subnest_share = sum(!!! rlang::sym(mkt_share))) %>%
            ungroup()
}

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

# share for two level nests
within_share_two_nest <- function(df, mkt_share, nest_share, subnest_share){
    output <- df %>%
                mutate(within_subnest = (!!rlang::sym(mkt_share)) /
                                            (!!rlang::sym(subnest_share)),
                       within_nest    = (!!rlang::sym(subnest_share)) /
                                            (!!rlang::sym(nest_share))
                    )
    return(output)
}

# share for one level nests
within_share_one_nest <- function(df, mkt_share, nest_share){

    output <- df %>%
                mutate(
                       within_nest    = (!!rlang::sym(mkt_share)) /
                                            (!!rlang::sym(nest_share))
                    )
    return(output)
}

create_shares <- function(df, market_id, mkt_share, outside_share = NULL,
                         nest_id = NULL, subnest_id = NULL){

    # generate outside share if needed
    if(is.null(outside_share)){
        print("Creating Outside Share...")
        df <- df %>%
                compute_outside_share(., mkt_share, market_id)
        print("Done!")
    }
    # generate nest share for one level nest
    if(!is.null(nest_id) && is.null(subnest_id)){
        print("Working with one layer of nests...")
        print(paste("Nest ID is:", nest_id))
        df <- df %>%
                gen_nest_share(., mkt_share, market_id, nest_id) %>%
                gen_within_share(., mkt_share, nest_share = "nest_share",
                                 subnest_share = NULL) %>%
            select(-nest_share)
    }

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
                select(-nest_share, -subnest_share)

    }


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
