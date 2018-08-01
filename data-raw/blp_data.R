# clean up blp_data
library(magrittr)
library(dplyr)

blp_1995 <- readr::read_csv("data-raw/blp_data.csv")

blp_1995 <- blp_1995 %>%
                rename(
                    model_name = model.name,
                    model_id  = model.id,
                    firm_id = firm.id,
                    time_id = cdid
                    ) %>%
                select(-id, -trend)

devtools::use_data(blp_1995, overwrite = TRUE, compress = 'xz')
