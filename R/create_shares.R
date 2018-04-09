compute_outside_share <- function(mkt_shares, market_id, data_frame) {

    # use pipe without loading package
    `%>%` <- magrittr::`%>%`

    print(paste("Grouping by Geographic ID:", market_id[["geog_id"]],
            sep = " "))
    print(paste("Grouping by Time ID      :", market_id[["time_id"]],
            sep = " "))

    # unpack market_ids to a vector so can group by easily
    markets <- unlist(market_id)

    df <- data_frame

    # generate outside shares
    df <- df %>%
            dplyr::group_by(!!! rlang::syms(markets)) %>%
            dplyr::mutate(inside_share = sum(!!! rlang::sym(mkt_shares))) %>%
            dplyr::mutate(outside_share = 1 - inside_share) %>%
            # clean up columns we don't want returned
            dplyr::select(-inside_share)
            #dplyr::select(-inside_share, -geog_id, -time_id)

    return(df)
}
