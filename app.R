# allow pipe
library(magrittr)

ui <- fluidPage(
  shiny::titlePanel("Corona incidences"),
  shiny::HTML("Official incidences of SARS-CoV-2 virus infections"),
  shiny::textOutput("last_update"),
  shiny::htmlOutput("lockdown_warning"),
  shiny::selectizeInput(
    inputId = "county",
    label = "County",
    multiple = FALSE,
    selected = "Jena",
    choices = "Jena"
  ),
  shiny::textOutput("county_data"),
  shiny::plotOutput(outputId = "plot"),
  shiny::HTML(
    "Incidence is the number of positive cases of the last 7 days per 100 000 people.",
    "<br>",
    "Source: <a href = 'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html'>RKI</a>.",
    "All statements without guarantee."
  )
  )

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  con <- DBI::dbConnect(RSQLite::SQLite(), "data.db")
  incidences <- dplyr::tbl(con, "incidences")
  counties <-
    incidences %>%
    dplyr::distinct(county) %>%
    dplyr::collect() %>%
    dplyr::pull(county)

  output$last_update <- shiny::renderText({
    paste0(
      "Last update: ",
      file.info("data.db")$ctime
    )
  })
  
  shiny::updateSelectizeInput(
    session, "county",
    choices = counties
  )
  
  output$county_data <- shiny::renderText({
    last_record <-
      incidences %>%
        dplyr::filter(county == !!input$county) %>%
        dplyr::arrange(desc(date)) %>%
        head(1) %>%
        dplyr::collect() %>%
        dplyr::mutate(
          date = date %>% as.POSIXct(origin = "1970-01-01", tz="UTC") %>% as.Date()
        ) %>%
        as.list()
    
    shiny::HTML(
      "Current Incidence: ",
      last_record$incidence %>% round(),
      " as of ",
      last_record$date %>% as.character()
    )
  })
  
  output$lockdown_warning <- shiny::renderUI({
    day <- lubridate::today()
    days <- c(
      day - 1,
      day - 2,
      day - 3
    ) %>%
      as.POSIXct() %>%
      as.numeric()

    lockdown_day_incidences <-
      incidences %>%
      dplyr::collect() %>%
      dplyr::filter(county == !!input$county & date >= min(!!days) & date <= max(!!days)) %>%
      dplyr::arrange(desc(date)) %>%
      dplyr::collect() %>%
      dplyr::mutate(is_lockdown = incidence >= 100)
    
    text <- "Calculation of lockdown trigger failed"
    
    if(nrow(lockdown_day_incidences) != length(days)) {
      text <- "No data available to calculate lockdown"
    } else if (lockdown_day_incidences$is_lockdown %>% table() %>% purrr::pluck("TRUE") %>% is.null() ) {
      text <- paste0(
        "No lockdown was triggered as of ",
        today() %>% as.character(),
        " according to <a href = 'https://www.gesetze-im-internet.de/ifsg/index.html'>ifsg</a>!"
      )
    } else if(lockdown_day_incidences$is_lockdown %>% table() %>% purrr::pluck("TRUE") == 3) {
      text <- paste0(
        "The lockdown was triggered as of ",
        today() %>% as.character(),
        " according to <a href = 'https://www.gesetze-im-internet.de/ifsg/index.html'>ifsg</a>!"
      )
    }
    
    shiny::HTML(paste(text))
  })
  
  output$plot <- shiny::renderPlot({
    incidences %>%
      dplyr::filter(county == !!input$county) %>%
      dplyr::collect() %>%
      dplyr::mutate(
        date = date %>% as.POSIXct(origin = "1970-01-01", tz="UTC") %>% as.Date()
      ) %>%
      ggplot2::ggplot(ggplot2::aes(date, incidence)) +
        ggplot2::geom_line(color = "#ca225e") +
        ggplot2::scale_x_date(expand = c(0,0), date_breaks = "1 month", date_labels = "%b %y") +
        ggplot2::theme_classic(base_size = 20) +
        ggplot2::labs(
          x = "",
          y = "Incidence"
        )
})
}

shiny::shinyApp(ui, server)

