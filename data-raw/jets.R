# tidy up jets data

library(magrittr)
library(dplyr)

jets <- readr::read_delim("data-raw/jets.txt", delim = "\t", col_names = FALSE) %>%
            select(-X9)

jets <- jets %>%
            rename(model_id = X1,
                   manufacturer_id = X2,
                   year = X3,
                   price = X4,
                   quantity = X5,
                   max_range = X6,
                   speed = X7,
                   cabin_volume = X8)

manu_ids <- c(1,2,3)
brands <- c("Bombardier", "Dassault", "Gulfstream")

brand_data <- data.frame(manu_ids, brands)

jets <- jets %>%
            left_join(brand_data, by = c("manufacturer_id" = "manu_ids"))

devtools::use_data(jets, overwrite = TRUE, compress = 'xz')
