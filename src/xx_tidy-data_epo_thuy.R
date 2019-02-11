library(tidyverse)
library(lubridate)
library(openxlsx)

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
    rename_all(str_to_lower) %>%
    rename(lab_event_id = event_id)

rm(labs1, labs2, labs3)

hgb <- labs %>%
    filter(lab == "Hgb") %>%
    inner_join(
        epo[c("encounter_id", "event_id", "clinical_event_datetime")],
        by = "encounter_id"
    ) %>%
    filter(
        lab_datetime >= clinical_event_datetime - days(8),
        lab_datetime <= clinical_event_datetime
    ) %>%
    group_by(encounter_id, event_id) %>%
    summarize_at("lab_result", max, na.rm = TRUE) %>%
    mutate(hgb_high = lab_result >= 10) %>%
    rename(hgb = lab_result)
    
iron_studies <- labs %>%
    filter(lab != "Hgb") %>%
    inner_join(
        epo[c("encounter_id", "event_id", "clinical_event_datetime")],
        by = "encounter_id"
    ) %>%
    filter(
        !is.na(lab_result),
        lab_datetime >= clinical_event_datetime - days(8),
        lab_datetime <= clinical_event_datetime
    ) %>%
    distinct(encounter_id, event_id, lab, .keep_all = TRUE) %>%
    select(encounter_id, event_id, lab, lab_result) %>%
    spread(lab, lab_result) %>%
    mutate(iron_studies = !is.na(`Ferritin Lvl`) | !is.na(Iron) | !is.na(TIBC) | !is.na(Transferrin) | !is.na(UIBC))

transfuse <- prbc %>%
    rename(prbc_event_id = event_id) %>%
    inner_join(
        epo[c("encounter_id", "event_id", "clinical_event_datetime")],
        by = "encounter_id"
    ) %>%
    filter(
        event_datetime >= clinical_event_datetime - days(8),
        event_datetime <= clinical_event_datetime
    ) %>%
    group_by(encounter_id, event_id) %>%
    summarize_at("event_datetime", min, na.rm = TRUE) %>%
    rename(transfuse_datetime = event_datetime) %>%
    mutate(transfused = !is.na(transfuse_datetime))

iron_dose <- iron %>%
    rename(med_event_id = event_id) %>%
    mutate_at(
        "route_event", 
        str_replace_all, 
        pattern = "IVPB",
        replacement = "IV"
    ) %>%
    inner_join(
        epo[c("encounter_id", "event_id", "clinical_event_datetime")],
        by = "encounter_id"
    ) %>%
    filter(
        med_datetime >= clinical_event_datetime - days(8),
        med_datetime <= clinical_event_datetime
    ) %>%
    group_by(encounter_id, event_id, route_event) %>%
    summarize_at("med_datetime", min, na.rm = TRUE) %>%
    spread(route_event, med_datetime) %>%
    mutate(iron_given = !is.na(IV) | !is.na(PO))
    
data_epo <- epo %>%
    left_join(hgb, by = c("encounter_id", "event_id")) %>%
    left_join(iron_studies, by = c("encounter_id", "event_id")) %>%
    left_join(transfuse, by = c("encounter_id", "event_id")) %>%
    left_join(iron_dose, by = c("encounter_id", "event_id")) %>%
    mutate_at(
        c("hgb_high", "iron_studies", "transfused", "iron_given"), 
        funs(coalesce(., FALSE))
    ) %>%
    select(
        fin,
        encounter_type,
        facility_event,
        nurse_unit_event,
        clinical_event_datetime,
        dose,
        route_event,
        hgb_high,
        iron_studies,
        iron_given,
        transfused
    )

write.xlsx(data_epo, "data/external/epo_data_fy19.xlsx")
