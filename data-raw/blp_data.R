# clean up blp_data
library(magrittr)
library(dplyr)

blp_data <- readr::read_csv("data-raw/blp_data.csv")

blp_data <- blp_data %>%
                rename(
                    model_name = model.name,
                    model_id  = model.id,
                    firm_id = firm.id,
                    time_id = cdid,
                    ) %>%
                select(-id, -trend)

devtools::use_data(blp_data, overwrite = TRUE, compress = 'xz')
