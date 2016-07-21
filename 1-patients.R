# 1-patients.R

library(edwr)
library(dplyr)
library(lubridate)

data.raw <- "data-raw"

BGTools::gzip_files(data.raw)

raw.patients <- read_data(data.raw, "patients") %>%
    as.patients() %>%
    filter(discharge.datetime < ymd("2016-07-01", tz = "US/Central"))

concat_encounters(raw.patients$pie.id)

raw.meds_sched <- read_data(data.raw, "meds_sched") %>%
    as.meds_sched()

ref <- data_frame(name = c("heparin", "ticagrelor", "warfarin"),
                  type = "med",
                  group = c("cont", "sched", "sched"))

tmp.heparin <- read_data(data.raw, "meds_cont") %>%
    as.meds_cont() %>%
    tidy_data(ref = ref, sched = raw.meds_sched) %>%
    calc_runtime() %>%
    summarize_data() %>%
    rename(heparin = med)

tmp.meds_sched <- tidy_data(raw.meds_sched, ref = ref)

tmp.ticag <- tmp.meds_sched %>%
    filter(med == "ticagrelor") %>%
    rename(ticag = med)

tmp.warf <- tmp.meds_sched %>%
    filter(med == "warfarin") %>%
    group_by(pie.id) %>%
    arrange(pie.id, med.datetime) %>%
    summarize(warf.start = first(med.datetime),
              warf.stop = last(med.datetime),
              num.warf = n())

data.overlap <- left_join(tmp.ticag, tmp.heparin, by = "pie.id") %>%
    left_join(tmp.warf, by = "pie.id") %>%
    mutate(tic_hep = if_else(med.datetime >= start.datetime &
                                med.datetime <= stop.datetime,
                            TRUE, FALSE, FALSE),
           tic_war = if_else(med.datetime >= warf.start &
                                 med.datetime <= warf.stop,
                             TRUE, FALSE, FALSE),
           war_hep = int_overlaps(interval(start.datetime, stop.datetime, "US/Central"),
                                  interval(warf.start, warf.stop, "US/Central"))) %>%
    group_by(pie.id) %>%
    summarize(tic_hep = sum(tic_hep),
              tic_war = sum(tic_war),
              war_hep = sum(war_hep))

data.patients <- filter(data.overlap, tic_hep > 0)

smry <- data.overlap %>%
    summarize(num_patients = n(),
              num_any_overlap = sum(tic_hep > 0),
              num_3days_overlap = sum(tic_hep >= 6))

readr::write_csv(data.overlap, "data-tidy/overlap.csv")
