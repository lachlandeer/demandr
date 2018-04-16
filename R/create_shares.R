## documentation needed
compute_outside_share <- function(df, mkt_shares, market_id) {

    # use pipe without loading package
    `%>%` <- magrittr::`%>%`

    # unpack market_ids to a vector so can group by easily
    markets <- unlist(market_id)

    # generate outside shares
    outside_share <- df %>%
            dplyr::group_by(!!! rlang::syms(markets)) %>%
            dplyr::mutate(inside_share = sum(!!! rlang::sym(mkt_shares))) %>%
            dplyr::mutate(outside_share = 1 - inside_share) %>%
            # ungroup the data and clean up columns we don't want returned
            ungroup() %>%
            dplyr::select(-inside_share)
            ## TODO: add asserts that shares are between zero and one

}

## dplyr approach to nest shares (testing)
gen_nest_shares <- function(df, mkt_shares, market_id, nest_id){
    # use pipe without loading package
    `%>%` <- magrittr::`%>%`

    # unpack market_ids to a vector so can group by easily
    markets <- unlist(market_id)

    mkt_and_nests <- c(markets, nest_id)

    nest_shares <- df %>%
            dplyr::group_by(!!! rlang::syms(mkt_and_nests)) %>%
            dplyr::mutate(nest_shares = sum(!!! rlang::sym(mkt_shares))) %>%
            ungroup()
}

## dplyr approach to subnest shares (testing)
gen_subnest_shares <- function(df, mkt_shares, market_id, nest_id, subnest_id){
    # use pipe without loading package
    `%>%` <- magrittr::`%>%`

    # unpack market_ids to a vector so can group by easily
    markets <- unlist(market_id)

    mkt_and_nests <- c(markets, nest_id, subnest_id)

    nest_shares <- df %>%
            dplyr::group_by(!!! rlang::syms(mkt_and_nests)) %>%
            dplyr::mutate(subnest_shares = sum(!!! rlang::sym(mkt_shares))) %>%
            ungroup()
}

gen_within_shares <- function(df, mkt_share, nest_share, subnest_share = NULL){
    # if one level of nesting
    if(is.null(subnest_share)){
        within_shares_one_nest(df, mkt_share, nest_share)
    }

    # if two levels of nesting
    if(!is.null(subnest_share)){
        within_shares_two_nest(df, mkt_share, nest_share, subnest_share)
    }
} # eof

# shares for two level nests
within_shares_two_nest <- function(df, mkt_share, nest_share, subnest_share){
    output <- df %>%
                mutate(within_subnest = (!!rlang::sym(mkt_share)) /
                                            (!!rlang::sym(subnest_share)),
                       within_nest    = (!!rlang::sym(subnest_share)) /
                                            (!!rlang::sym(nest_share))
                    )
    return(output)
}

# shares for one level nests
within_shares_one_nest <- function(df, mkt_share, nest_share){
    output <- df %>%
                mutate(
                       within_nest    = (!!rlang::sym(mkt_share)) /
                                            (!!rlang::sym(nest_share))
                    )
    return(output)
}

create_shares <- function(){

    # generate outside share if needed
    if(is.null(outside_share)){
        df <- df %>%
                compute_outside_share(., mkt_shares, market_id)
    }

    # generate nest shares for one level nest
    if(!is.null(nest_id) && is.null(subnest_id)){
        df <- df %>%
                gen_nest_shares(., mkt_share, market_id, nest_id) %>%
                gen_within_shares(., mkt_share, nest_share, subnest_share)
    }

    if(!is.null(nest_id) && !is.null(subnest_id)){
        df <- df %>%
                gen_nest_shares(., mkt_share, market_id, nest_id) %>%
                gen_subnest_shares(., mkt_shares, market_id, nest_id, subnest_id)
                gen_within_shares(., mkt_share, nest_share, subnest_share)
    }

    return(df)
}
