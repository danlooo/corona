library(tidyverse)
library(readxl)
library(RSQLite)
library(DBI)
library(lubridate)

data_url <- paste0(
  "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/",
  "Daten/Fallzahlen_Kum_Tab.xlsx?__blob=publicationFile"
)
data_excel_path <- "/app/data.xlsx"
data_sqlite_path <- "/app/data.db"

#' Parse and fix RKI date formats to SQLite friendly UNIX epoch
parse_date <- function(x) {
  res <- x %>% lubridate::parse_date_time(c("dmy", "mdy")) %>% as.POSIXct() %>% as.numeric()
  
  if(is.na(res)) {
    res <- (as.Date("2021-04-16") + (as.integer(x) - 44302)) %>% as.POSIXct() %>% as.numeric()
    return(res)
  }
  
  if(is.numeric(res)) {
    return(res)
  }
}

capita <- read_csv("/app/capita.csv")

download.file(url = data_url, destfile = data_excel_path)

incidences <-
  data_excel_path %>%
  read_excel(skip = 4, sheet = "LK_7-Tage-Inzidenz") %>%
  rename(county=LK) %>%
  select(-...1, -LKNR) %>%
  pivot_longer(-county, names_to = "old_date", values_to = "incidence") 

incidences <-
  incidences %>%
  distinct(old_date) %>%
  mutate(date = old_date %>% map_dbl(parse_date)) %>%
  left_join(incidences) %>%
  select(-old_date) %>%
  mutate(county = county %>% str_remove("^(SK|LK) "))

incidences <-
  incidences %>%
  left_join(capita) %>%
  transmute(
    county, 
    date,
    incidence = (incidence / (capita / 100e3)) %>% round(2)
  )


if (file.exists(data_sqlite_path)) {
  file.remove(data_sqlite_path)
}

con <- dbConnect(SQLite(), data_sqlite_path)
dbWriteTable(con, "incidences", incidences)
