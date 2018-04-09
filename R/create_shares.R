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
            dplyr::select(-inside_share, -geog_id, -time_id)
            ## TODO: add asserts that shares are between zero and one

}
