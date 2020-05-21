library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')

eruptions$start_date <- as.Date(with(eruptions, paste(start_year, start_month, start_day,sep="-")), "%Y-%m-%d")
eruptions$end_date <- as.Date(with(eruptions, paste(end_year, end_month, end_day, sep = "-")), "%Y-%m-%d")


eruptions <- eruptions %>%
                      group_by(volcano_number) %>%
                      mutate(max_vei = max(vei, na.rm = TRUE),
                             eruption_duration = end_date - start_date) %>%
                      ungroup()

eruptions <- eruptions %>%
                      group_by(volcano_number) %>%
                      filter(start_date == max(start_date, na.rm = TRUE)) %>%
                      ungroup() %>%
                      select(volcano_number, eruption_category, start_date, max_vei,
                             eruption_duration, vei)

volcanos <- volcano %>%
                      left_join(eruptions, by = "volcano_number")

volcanos %>% filter(last_eruption_year != lubridate::year(start_date)) %>%
  select(last_eruption_year, start_date)
  


                      









# erup_events <- events %>%
#   left_join(eruptions, by=c("volcano_number", "volcano_name", "eruption_number"))
# 
# erup_event_volcano <- erup_events %>%
#   left_join(volcano, by = c("volcano_number", "volcano_name"), suffix = c("_erruption", "_volcano"))
# 
# sulfur$year <- floor(sulfur$year)
# 
# sulfur_by_year <- sulfur %>%
#   group_by(year) %>%
#   summarize(mean_neem = mean(neem, na.rm = TRUE),
#             mean_wdc = mean(wdc, na.rm = TRUE))
# 
# erup_event_volcano <- erup_event_volcano %>%
#   left_join(sulfur_by_year, by = c("end_year"="year"))
# 
# erup_event_volcano <- erup_event_volcano %>%
#   left_join(tree_rings, by = c("end_year"="year"))
# 
# erup_event_volcano %>% is.na() %>% colSums()
# 
# erup_event_volcano_map <- erup_event_volcano %>% select(volcano_number,
#                                                     volcano_name,
#                                                     eruption_number,
#                                                     eruption_start_year,
#                                                     event_number,
#                                                     event_type,
#                                                     eruption_category,
#                                                     vei,
#                                                     start_year,
#                                                     latitude_erruption,
#                                                     longitude_erruption,
#                                                     population_within_10_km,
#                                                     primary_volcano_type)


volcanos$population_within_10_km[is.na(volcanos$population_within_10_km)] <- 0

volcanos$primary_volcano_type[is.na(volcanos$primary_volcano_type)] <- "Unknown"
volcanos %>% is.na() %>% colSums()
ui <- bootstrapPage(
  #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("map", width = "100%", height = 400),
  
  absolutePanel(id = "controls", class = "panel panel-default",
               top = 500, left = 55, width = 250, fixed=TRUE,
               draggable = TRUE, height = "auto",
               
                pickerInput("category", label = "Select an Evidence Category:",
                            choices = c("All Categories", 
                                          unique(volcanos$evidence_category)),
                            options = list(
                              
                              `live-search` = TRUE))))


  
                

server <- function(input, output, session) {
  
  bins = c(0,3,6,9,12,15)
  cv_pal <- colorBin("Blues", domain = log(volcanos$population_within_10_km), bins = bins)
  
  filteredData <- reactive({
    if (input$category == "All Categories") {
      volcanos
    } else {
      filter(volcanos, evidence_category == input$category)
    }
  })
  
  
  
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
        addMarkers(~longitude, ~latitude,
                   clusterOptions = markerClusterOptions(),
                   label = sprintf("<br/>Volcano Name: %s<br/>Tectonic Settings: %s<br/>Major Rock: %s<br/>VEI: %g<br/>Maximum VEI: %g<br/>Eruption Duration: %g", filteredData()$volcano_name,
                                   filteredData()$tectonic_settings,
                                   filteredData()$major_rock_1,
                                   filteredData()$vei,
                                   filteredData()$max_vei,
                                   filteredData()$eruption_duration) %>% lapply(htmltools::HTML),
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#cc4c02"),
                     textsize = "15px", direction = "auto")) %>%
      addCircleMarkers(radius = ~log(population_within_10_km),
                       weight = 1,
                       stroke = FALSE, fillOpacity = 0.5
                       ) %>%
      addLegend("bottomright", pal = cv_pal, values = ~log(population_within_10_km),
                title = "<small>Population within 10 km</small>")
  })
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addMarkers(~longitude, ~latitude,
                 clusterOptions = markerClusterOptions(),
                 label = sprintf("<strong>Volcano Name: %s</strong><br/>Tectonic Settings: %s<br/>Major Rock: %s<br/>VEI: %g<br/>Maximum VEI: %g<br/>Eruption Duration: %g", filteredData()$volcano_name,
                                 filteredData()$tectonic_settings,
                                 filteredData()$major_rock_1,
                                 filteredData()$vei,
                                 filteredData()$max_vei,
                                 filteredData()$eruption_duration) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#cc4c02"),
                   textsize = "15px", direction = "auto")) %>%
      addCircleMarkers(radius = ~log(population_within_10_km),
                       weight = 1,
                       stroke = FALSE, fillOpacity = 0.5)
  })
}

shinyApp(ui, server)