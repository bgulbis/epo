library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw"

# run MBO query
#   * Patients - by Order - no Building
#       - Facility (Curr): HH HERMANN
#       - Mnemonic (Primary Generic) FILTER ON: epoetin alfa

pts <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() 

mbo_id <- concat_encounters(pts$millennium.id)

# run MBO queries
#   * Orders Meds - Details - by Med
#       - Mnemonic (Primary Generic) FILTER ON: epoetin alfa

orders <- read_data(dir_raw, "orders", FALSE) %>%
    as.order_detail(
        extras = list(
            "order.stop" = "Date and Time - Discontinue Effective"
        )
    ) %>%
    filter(
        floor_date(order.datetime, "month") == mdy("2/1/2018", tz = "US/Central")
    ) 

orders_sum <- orders %>%
    count(ingredient.dose) %>%
    mutate(pct = n / nrow(orders) * 100) %>%
    arrange(desc(pct))

write.csv(orders_sum, "data/external/epo_orders.csv", row.names = FALSE)

ggplot(orders, aes(x = ingredient.dose)) +
    geom_bar()
