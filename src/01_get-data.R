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
