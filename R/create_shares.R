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
