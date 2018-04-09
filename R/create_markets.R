# this function maps market ids into a list so they are
# consistently unpacked

create_markets <- function(geog_id, time_id){
    markets <- list()

    markets[["geog_id"]] <- geog_id
    markets[["time_id"]] <- time_id

    return(markets)
}
