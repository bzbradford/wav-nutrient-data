library(tidyverse)
library(shiny)
library(shinyBS)
library(leaflet)
library(plotly)

options(warn = -1)

# Functions ---------------------------------------------------------------

colorize <- function(text, color = text) {
  HTML(paste0("<span style='color: ", color, "'>", text, "</span>"))
}



# Load data ---------------------------------------------------------------

data <- read_csv("data/tp-data.csv")
years <- unique(data$Year)
station_years <- data %>%
  distinct(StationID, Year) %>%
  group_by(StationID) %>%
  summarise(YearsActive = paste(unlist(Year), collapse = ", "))
stations <- data %>%
  group_by(Year, StationID, StationName, Latitude, Longitude, NumObs) %>%
  summarise(MeanTP = round(mean(TP, na.rm = T), 3), .groups = "drop") %>%
  mutate(LogTP = log1p(MeanTP)) %>%
  left_join(station_years, by = "StationID") %>%
  select(Year, YearsActive, everything())
phoslimit <- 0.075 # mg/L, ppm



# UI ----------------------------------------------------------------------

ui <- fluidPage(
  title = "WAV Nutrient Data Dashboard",
  
  tags$head(
    includeHTML("google-analytics.html"),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$style("
      body {
        font-family: 'Lato', sans-serif;
      }
      
      .container-fluid {
        max-width: 1000px;
      }
      
      .panel-body {
        padding: 0px;
      }
      
      .has-feedback .form-control {
        padding-right: 12px;
        min-width: 5em;
      }
    ")
  ),
  
  div(
    align = "center",
    style = "margin-top: 1em;",
    a(img(src = "wav-logo-color.png", height = "100px"), href = "https://wateractionvolunteers.org", target = "_blank")
  ),
  
  br(),
  
  h2("Total Phosphorus Nutrient Monitoring Data", align = "center"),
  
  br(),
  
  p(
    div(style = "float: left; margin-right: 1em;", strong("Select data year:")),
    radioButtons(
      inputId = "year",
      label = NULL,
      choices = years,
      selected = max(years),
      inline = T
    )
  ),
  
  uiOutput("stationSelectUI", style = "z-index: 1100;"),
  uiOutput("mapUI"),
  uiOutput("plotUI"),
  
  fluidRow(
    column(12,
      h4("Exceedance criteria"),
      p("The shaded horizontal band on the plot represents the 90% confidence interval for the median total phosphorus (TP) at this site (if more than one month of data was collected). This means that, given the TP concentrations measured this year, there is about an 90% chance that the true median total phosphorus concentration falls somewhere between those lines. We know that TP in streams varies quite a bit, so individual samples could be higher or lower than the confidence interval."),
      p("A stream site is considered 'Criteria Exceeded' and the confidence interval band will be shaded", colorize("red"), "if: 1) the lower 90% confidence limit of the sample median exceeds the state TP criterion of", phoslimit, "mg/L or 2) there is corroborating WDNR biological data to support an adverse response in the fish or macroinvertebrate communities. If there is insufficient data for either of these requirements, more data will need to be collected in subsequent years before a decision can be made. A site is designated as 'Watch Waters' if the total phosphorus state criterion concentration falls within the confidence limit or additional data are required, and a site is considered to have 'Met Criteria' if the upper limit of the confidence interval does not exceed the criterion (shaded confidence interval band will be", HTML(paste0(colorize("teal"), ").")), "This year, many sites are assigned 'Watch Waters' because fewer than six samples were collected. Nevertheless, these total phosphorus measurements will still improve our understanding of stream health at this site."),
      br(),
      h4("Why Phosphorus?"),
      p("Phosphorus is an essential nutrient responsible for plant growth, but it is also the most visible, widespread water pollutant in lakes. Small increases in phosphorus levels can bring about substantial increases in aquatic plant and algae growth, which in turn can reduce the recreational use and biodiversity. When the excess plants die and are decomposed, oxygen levels in the water drop dramatically which can lead to fish kills. Additionally, one of the most common impairments in Wisconsin’s streams is excess sediment that covers stream bottoms. Since phosphorus moves attached to sediments, it is intimately connected with this source of pollution in our streams. Phosphorus originates naturally from rocks, but its major sources in streams and lakes today are usually associated with human activities: soil erosion, human and animal wastes, septic systems, and runoff from farmland or lawns. Phosphorus-containing contaminants from urban streets and parking lots such as food waste, detergents, and paper products are also potential sources of phosphorus pollution from the surrounding landscape. The impact that phosphorus can have in streams is less apparent than in lakes due to the overall movement of water, but in areas with low velocity, where sediment can settle and deposit along the bottom substrate, algae blooms can result."),
      br(),
      h4("Volunteer Monitoring Protocol"),
      p("To assess in-stream phosphorus levels, WAV volunteers collected water samples that were analyzed for total phosphorus (TP) at the State Lab of Hygiene during the growing season. Following Wisconsin Department of Natural Resources (WDNR) methods, four to six phosphorus water samples were collected at each monitoring site - one per month for up to each of the six months during the growing season. The monthly water samples were collected approximately 30 days apart and no samples were collected within 15 days of one another. Samples at several sites were collected every two weeks. The monthly values are an average of the biweekly sample results."),
      br(),
      h4("About Water Action Volunteers"),
      p(a("Water Action Volunteers", href = "https://wateractionvolunteers.org", target = "_blank"), "is an organization run by the UW-Madison Division of Extension and the Wisconsin Department of Natural Resources. The aim is to engage volunteers across the state to perform repeated, periodic water quality measurements of local streams to improve our understanding of stream health and to inform conservation and restoration efforts."),
    )
  ),
  
  br(),
  hr(),
  p(
    style = "color: grey; font-size: smaller; font-style: italic;",
    align = "center",
    "Dashboard developed by",
    a("Ben Bradford", href = "https://github.com/bzbradford", target = "_blank", .noWS = "after"),
    ", UW-Madison Entomology", br(),
    paste("Last updated:", format(file.info(".")$mtime, "%Y-%m-%d")), br(),
    a("Source code", href = "https://github.com/bzbradford/wav-nutrient-data", target = "_blank")
  )
  
)



# Server ------------------------------------------------------------------

server <- function(input, output, sessions) {
  
  ## On startup ----
  
  random_stn <- stations %>%
    filter(Year == max(years)) %>%
    pull(StationID) %>%
    sample(1)
  
  
  
  ## Reactive values ----
  
  avail_stations <- reactive({
    req(input$year)
    
    stations %>%
      filter(Year == input$year)
  })
  
  station_list <- reactive({
     avail_stations() %>%
      mutate(Label = paste0("Station ", StationID, ": ", StationName)) %>%
      arrange(StationID) %>%
      select(Label, StationID) %>%
      deframe() %>%
      as.list()
  })
  
  cur_stn <- reactive({
    req(input$station)
    
    avail_stations() %>%
      filter(StationID == input$station)
  })
  
  # select station data
  stn_data <- reactive({
    req(input$year)
    req(input$station)
    
    data %>%
      filter(Year == input$year, StationID == input$station) %>%
      arrange(Date)
  })
  
  phos_estimate <- reactive({
    vals <- na.omit(stn_data()$TP)
    log_vals <- log(vals)
    n <- length(vals)
    meanp <- mean(log_vals)
    se <- sd(log_vals) / sqrt(n)
    tval <- qt(p = 0.90, df = n - 1)
    
    params <- list(
      mean = meanp,
      median = median(log_vals),
      lower = meanp - tval * se,
      upper = meanp + tval * se
    )
    
    params <- lapply(params, exp)
    params <- lapply(params, round, 3)
    params["n"] <- n
    params
  })
  
  
  
  ## Station select ----
  
  output$stationSelectUI <- renderUI({
    fluidRow(
      column(12,
        selectInput(
          inputId = "station",
          label = "Select nutrient monitoring station:",
          choices = list("Select a station" = NULL),
          width = "100%"
        ),
        style = "z-index: 1001;"
      )
    )
  })
  
  observeEvent(input$year, {
    stations <- station_list()
    if (is.null(input$station)) {
      selected <- random_stn
    } else if (input$station %in% stations) {
      selected <- input$station
    } else {
      selected <- stations[1]
    }
    updateSelectInput(
      inputId = "station",
      choices = stations,
      selected = selected
    )
  })
  
  
  
  ## Map ----
  
  stationPal <- colorNumeric(
    palette = "RdYlGn",
    domain = log1p(c(0, 0.2)),
    na.color = "#c5050c",
    reverse = TRUE)
  
  output$mapUI <- renderUI({
    bsCollapse(
      id = "map_collapse",
      open = "map",
      bsCollapsePanel(
        title = "Station location map",
        value = "map",
        leafletOutput("map"),
        div(style = "margin: 0.5em 1em; 0.5em 1em;", align = "center",
          p(em(HTML(paste0("Currently selected station is highlighted. Click on any other station to select it, or choose from the list above.")))),
          p(
            actionButton("zoom_in", "Zoom to selected site"),
            actionButton("reset_zoom", "Zoom out to all sites"),
            actionButton("random_site", "Random site")
          )
        )
      )
    )
  })
  
  basemaps <- c(
    "Grey Canvas",
    "Open Street Map",
    "ESRI Topo Map"
  )
  
  output$map <- renderLeaflet({
    leaflet() %>%
      fitBounds(
        lat1 = min(stations$Latitude),
        lat2 = max(stations$Latitude),
        lng1 = min(stations$Longitude),
        lng2 = max(stations$Longitude)
      ) %>%
      addTiles(group = basemaps[2]) %>%
      addProviderTiles(providers$CartoDB.Positron, group = basemaps[1]) %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = basemaps[3]) %>%
      addMapPane("all_points", zIndex = 420) %>%
      addMapPane("cur_point", zIndex = 430) %>%
      addLayersControl(
        baseGroups = basemaps,
        options = layersControlOptions(collapsed = T)
      ) %>%
      htmlwidgets::onRender("
        function() {
          $('.leaflet-control-layers-list').prepend('<b>Basemap:</b>');
        }
      ")
  })
  
  pointLabels <- function(df) {
    with(df, lapply(paste0(
      "<b>Station ID:</b> ", StationID, "<br>",
      "<b>Station Name:</b> ", gsub("\n", "<br>&nbsp;&nbsp;&nbsp;&nbsp;", str_wrap(StationName, width = 40)), "<br>",
      "<b>Years covered:</b> ", YearsActive, "<br>",
      "<b>", input$year, ":</b> ", MeanTP, " ppm total phosphorus (", NumObs, " observations)"),
      HTML))
  }
  
  observeEvent(list(station_list(), input$map_collapse), {
    req(input$year)
    
    df <- stations %>%
      filter(Year == input$year)
    
    leafletProxy("map") %>%
      clearGroup("all_points") %>%
      addCircleMarkers(
        data = df,
        lat = ~Latitude,
        lng = ~Longitude,
        label = ~pointLabels(df),
        group = "all_points",
        layerId = ~ StationID,
        options = pathOptions(pane = "all_points"),
        radius = 6,
        weight = 0.5,
        color = "black",
        fillColor = ~stationPal(LogTP),
        fillOpacity = 0.6
      )
  })
  
  # handle displaying selected logger in green
  observeEvent(list(cur_stn(), input$map_collapse), {
    req(cur_stn())
    
    leafletProxy("map") %>%
      clearGroup("cur_point") %>%
      addCircleMarkers(
        data = cur_stn(),
        lat = ~Latitude,
        lng = ~Longitude,
        label = ~pointLabels(cur_stn()),
        layerId = ~StationID,
        group = "cur_point",
        options = pathOptions(pane = "cur_point"),
        radius = 7,
        weight = 2,
        opacity = 1,
        color = "black",
        fillColor = ~stationPal(LogTP),
        fillOpacity = 1
      )
  })
  
  observeEvent(input$zoom_in, {
    leafletProxy("map") %>%
      setView(
        lat = cur_stn()$Latitude,
        lng = cur_stn()$Longitude,
        zoom = 10
      )
  })
  
  observeEvent(input$reset_zoom, {
    leafletProxy("map") %>%
      fitBounds(
        lat1 = min(stations$Latitude),
        lat2 = max(stations$Latitude),
        lng1 = min(stations$Longitude),
        lng2 = max(stations$Longitude)
      )
  })
  
  observeEvent(input$random_site, {
    stn_id <- sample(avail_stations()$StationID, 1)
    stn <- avail_stations() %>%
      filter(StationID == stn_id)
    updateSelectInput(
      inputId = "station",
      selected = stn_id
    )
    leafletProxy("map") %>%
      setView(
        lat = stn$Latitude,
        lng = stn$Longitude,
        zoom = 10
      )
  })
  
  # get clicked station location and select it
  observe({
    updateSelectInput(
      inputId = "station",
      selected = input$map_marker_click
    )
  })
  
  
  
  ## Plot ----
  
  output$plotUI <- renderUI({
    bsCollapse(
      bsCollapsePanel(
        title = "Total phosphorus data",
        value = "plot",
        plotlyOutput("plot"),
        div(style = "margin: 0.5em 1em; 0.5em 1em;", align = "center",
          p(em("The dashed line on this plot indicates the total phosphorus state exceedance level of 0.075 mg/L (ppm). If more than one month of data was collected, the median and 90% confidence interval for the true total phosphorus level are displayed as a horizontal band."))
        )
      ),
      open = "plot"
    )
  })
  
  hline <- function(y = 0, color = "black") {
    list(
      type = "line",
      x0 = 0,
      x1 = 1,
      xref = "paper",
      y0 = y,
      y1 = y,
      line = list(color = color, dash = "dash")
    )
  }
  
  rect <- function(ymin, ymax, color = "red") {
    list(
      type = "rect",
      fillcolor = color,
      line = list(color = color),
      opacity = 0.1,
      y0 = ymin,
      y1 = ymax,
      xref = "paper",
      x0 = 0,
      x1 = 1,
      layer = "below"
    )
  }
  
  output$plot <- renderPlotly({
    req(input$year)
    req(input$station)
    
    plot_title <- str_trunc(paste0("Station ", cur_stn()$StationID, ": ", cur_stn()$StationName), width = 80)
    df <- stn_data() %>%
      mutate(Exceedance = factor(
        ifelse(is.na(TP), "No data", ifelse(TP >= phoslimit, "High", "OK")),
        levels = c("OK", "High", "No data"))) %>%
      drop_na(TP)
    date_range <- as.Date(paste0(input$year, c("-05-01", "-10-31")))
    outer_months <- as.Date(paste0(input$year, c("-04-30", "-11-1")))
    data_dates <-  as.Date(paste(input$year, 5:10, 15, sep = "-"))
    all_dates <-  c(outer_months, data_dates)
    yrange <- c(0, max(0.25, max(df$TP, na.rm = T)))
    
    phos_params <- tibble(
      Date = all_dates,
      Lower = phos_estimate()$lower,
      Upper = phos_estimate()$upper,
      Median = phos_estimate()$median
    )
    
    
    # no confidence invervals if only one month of data
    if (phos_estimate()$n > 1) {
      ci_color <- ifelse(phos_estimate()$upper >= phoslimit, "red", "teal")
      
      plt <- plot_ly(phos_params) %>%
        add_lines(
          x = ~Date,
          y = ~Upper,
          name = "Upper 90% CI",
          xperiod = "M1",
          xperiodalignment = "middle",
          opacity = 0.5,
          line = list(color = ci_color, width = 0.5)
        ) %>%
        add_lines(
          x = ~Date,
          y = ~Median,
          name = "Median",
          xperiod = "M1",
          xperiodalignment = "middle",
          opacity = 0.5,
          line = list(color = "darkblue", width = 2)
        ) %>%
        add_lines(
          x = ~Date,
          y = ~Lower,
          name = "Lower 90% CI",
          xperiod = "M1",
          xperiodalignment = "middle",
          opacity = 0.5,
          line = list(color = ci_color, width = 0.5)
        )
      
      shapes <- list(
        rect(phos_estimate()$lower, phos_estimate()$upper, ci_color),
        hline(phoslimit)
        )
    } else {
      plt <- plot_ly()
      shapes <- hline(phoslimit)
    }
    
    plt <- plt %>%
      add_trace(
        data = df,
        x = ~Date,
        y = ~TP,
        type = "bar",
        text = ~TP,
        textposition = "auto",
        color = ~Exceedance,
        colors = "Set2",
        width = 0.5 * 1000 * 60 * 60 * 24 * 30,
        xperiod = "M1",
        xperiodalignment = "middle",
        marker = list(
          line = list(color = "rgb(8,48,107)", width = 1)
        ),
        textfont = list(color = "black"),
        hovertemplate = "Measured TP: %{y:.3f}<extra></extra>"
      ) %>%
      layout(
        title = plot_title,
        showlegend = F,
        xaxis = list(
          title = "",
          type = "date",
          tickformat = "%B<br>%Y",
          dtick = "M1",
          ticklabelmode = "period",
          range = date_range),
        yaxis = list(
          title = "Total phosphorus (ppm)",
          zerolinecolor = "lightgrey",
          range = yrange),
        legend = list(
          traceorder = "reversed"
        ),
        hovermode = "x unified",
        margin = list(t = 50),
        shapes = shapes
      )
    
    plt
  })
}



# App ---------------------------------------------------------------------

shinyApp(ui, server)
