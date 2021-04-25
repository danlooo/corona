install.packages("devtools")

pkgs <- c(
  "readxl",
  "RSQLite",
  "DBI",
  "lubridate"
)

devtools::install_cran(pkgs)