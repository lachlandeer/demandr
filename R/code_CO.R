
### --- Generate aggregate market shares per nest and subnest

gen_nest_shares <- function(mkt_shares,market_id,nest_id){
  t_id <- market_id[["time_id"]]
  geo_id <- market_id[["geog_id"]]
  nest_shares <- aggregate(mkt_shares, by=list(t_id=t_id,geo_id=geo_id,nest_id=nest_id), FUN=sum)
    # sum individual-observation shares by nest within markets
  obs <- left_join(as.data.frame(cbind(t_id,geo_id,nest_id,deparse.level = 1)),nest_shares,by=c("t_id","geo_id","nest_id"))
    # expand sum of ahres to match number of obs in data
  nest_shares <- obs[[ncol(nest_shares)]]
    # export numerical vector of aggregate nest shares
}

gen_subnest_shares <- function(mkt_shares,market_id,nest_id,subnest_id){
  t_id <- market_id[["time_id"]]
  geo_id <- market_id[["geog_id"]]
  subnest_shares <- aggregate(mkt_shares, by=list(t_id=t_id,geo_id=geo_id,nest_id=nest_id,subnest_id=subnest_id), FUN=sum)
  obs <- left_join(as.data.frame(cbind(t_id,geo_id,nest_id,subnest_id,deparse.level = 1)),subnest_shares,by=c("t_id","geo_id","nest_id","subnest_id"))
  subnest_shares <- obs[[ncol(subnest_shares)]]
}

### --- Generate logged nest and subnest share ratios

gen_within_subnest_logratios <- function(mkt_shares,subnest_shares){
  within_subnest_logratios <- log(mkt_shares/subnest_shares)
}

gen_within_nest_logratios <- function(subnest_shares,nest_shares){
  within_nest_logratios <- log(subnest_shares/nest_shares)
}










