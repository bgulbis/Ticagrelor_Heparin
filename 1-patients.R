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

ref <- data_frame(name = c("heparin", "ticagrelor"),
                  type = "med",
                  group = c("cont", "sched"))

tmp.heparin <- read_data(data.raw, "meds_cont") %>%
    as.meds_cont() %>%
    tidy_data(ref = ref, sched = raw.meds_sched) %>%
    calc_runtime() %>%
    summarize_data() %>%
    rename(heparin = med)

tmp.ticag <- tidy_data(raw.meds_sched, ref = ref) %>%
    rename(ticag = med)

data.overlap <- left_join(tmp.ticag, tmp.heparin, by = "pie.id") %>%
    mutate(concur = if_else(med.datetime >= start.datetime &
                                med.datetime <= stop.datetime,
                            TRUE, FALSE, FALSE)) %>%
    group_by(pie.id) %>%
    summarize(concur = sum(concur))
