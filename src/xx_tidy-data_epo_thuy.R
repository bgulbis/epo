library(tidyverse)
library(lubridate)

dir_raw <- "data/tidy/thuy"
tz <- "US/Central"

get_data <- function(path, pattern, tz) {
    list.files(path, pattern, full.names = TRUE) %>%
        purrr::map_df(
            readr::read_csv,
            locale = readr::locale(tz = tz)
        ) %>%
        dplyr::rename_all(stringr::str_to_lower)
}

epo <- get_data(dir_raw, "epo", tz) 
# labs <- get_data(dir_raw, "lab", tz) 
iron <- get_data(dir_raw, "iron", tz) 
prbc <- get_data(dir_raw, "prbc", tz) 

labs1 <- read_csv("data/tidy/thuy/lab_events_2019-01.csv", locale = locale(tz = tz))
labs2 <- read_csv("data/tidy/thuy/lab_events_2018-10.csv", locale = locale(tz = tz))
labs3 <- read_csv("data/tidy/thuy/lab_events_2018-07.csv", locale = locale(tz = tz)) %>%
    mutate_at("LAB_RESULT", as.numeric)

labs <- bind_rows(labs1, labs2, labs3) %>%
    rename_all(str_to_lower)

rm(labs1, labs2, labs3)