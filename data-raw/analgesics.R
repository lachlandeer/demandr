# tidy up analgesics data

library(magrittr)
library(dplyr)

analgesics <- readr::read_delim("data-raw/analgesics.csv", delim = "\t")

analgesics <- analgesics %>%
                rename(store_id = store,
                       brand_id = brand,
                       sales = sales_,
                       customers = count,
                       price = price_,
                       promotion = prom_,
                       wholesale_price = cost_) %>%
                mutate(promotion = if_else(promotion > 0, TRUE, FALSE))

devtools::use_data(analgesics, overwrite = TRUE, compress = 'xz')
