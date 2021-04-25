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

download.file(url = data_url, destfile = data_excel_path)

incidences <-
  data_excel_path %>%
    read_excel(skip = 4, sheet = "LK_7-Tage-Inzidenz") %>%
    rename(county=LK) %>%
    select(-...1, -LKNR) %>%
    pivot_longer(-county, names_to = "date", values_to = "incidence") %>%
    mutate(
      county = county %>% str_remove("^(SK|LK) "),
      # character to unix epoch (SQLite does not support dates)
      date = date %>% lubridate::parse_date_time(c("dmy", "mdy")) %>% as.POSIXct() %>% as.numeric()
    )

if (file.exists(data_sqlite_path)) {
  file.remove(data_sqlite_path)
}

con <- dbConnect(SQLite(), data_sqlite_path)
dbWriteTable(con, "incidences", incidences)
