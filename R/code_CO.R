gen_nest_shares <- function(mkt_shares,market_id,nest_id){
  t_id <- market_id[["time_id"]]
  geo_id <- market_id[["geog_id"]]
  nest_shares <- aggregate(mkt_shares, by=list(t_id=t_id,geo_id=geo_id,nest_id=nest_id), FUN=sum)
  obs <- left_join(as.data.frame(cbind(t_id,geo_id,nest_id,deparse.level = 1)),nest_shares,by=c("t_id","geo_id","nest_id"))
  nest_shares <- obs[[ncol(nest_shares)]]
}

gen_subnest_shares <- function(mkt_shares,market_id,nest_id,subnest_id){
  t_id <- market_id[["time_id"]]
  geo_id <- market_id[["geog_id"]]
  subnest_shares <- aggregate(mkt_shares, by=list(t_id=t_id,geo_id=geo_id,nest_id=nest_id,subnest_id=subnest_id), FUN=sum)
  obs <- left_join(as.data.frame(cbind(t_id,geo_id,nest_id,subnest_id,deparse.level = 1)),subnest_shares,by=c("t_id","geo_id","nest_id","subnest_id"))
  subnest_shares <- obs[[ncol(subnest_shares)]]
}

