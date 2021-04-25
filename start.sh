#!/usr/bin/env sh

Rscript update_db.R
service cron start
R -e "shiny::runApp('/app', host = '0.0.0.0', port = 80)"