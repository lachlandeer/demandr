# clean up eurocars data

library(magrittr)
library(dplyr)


eurocars2 <- haven::read_dta("data-raw/eurocars.dta")
eurocars <- readxl::read_xlsx("data-raw/eurocars.xlsx")

eurocars <- eurocars %>%
                rename(year = ye,
                       market_id = ma,
                       model_id  = co,
                       model_id2 = zcode,
                       origin_id = org,
                       location_id = loc,
                       class_id = cla,
                       domestic  = home,
                       firm_id = frm,
                       qty_sold = qu,
                       cylinders = cy,
                       kilowatts = hp,
                       weight = we,
                       n_seats = pl,
                       n_doors = do,
                       length = le,
                       width = wi,
                       height = he,
                       fueleff_90 = li1,
                       fueleff_120 = li2,
                       fueleff_city = li3,
                       fueleff_avg = li,
                       max_speed = sp,
                       accel = ac,
                       price = pr,
                       price_euro = eurpr,
                       exchrate_exp = avexr,
                       exchrate_imp = avdexr,
                       cpi_exp = avcpr,
                       ppi_exp = avppr,
                       cpi_imp = avdcpr,
                       ppi_imp = avdppr,
                       population = pop
                       ) %>%
                select(-brd, -princ, -xexr, -exppr, -engdp, -ergdp, -engdpc, -ergdpc)

devtools::use_data(eurocars, overwrite = TRUE, compress = 'xz')
