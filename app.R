library(shiny)
library(sf)
library(dplyr)
library(leaflet)

# Load data
all_years_1km <- readRDS("all_years_1km.rds")
all_years_100m <- readRDS("all_years_100m.rds") |> 
  mutate(
    DEA2014_nm = as.character(DEA2014_nm),
    LGD2014_nm = as.character(LGD2014_nm),
    const24_nm = as.character(const24_nm)
  )
all_years_mixed <- readRDS("all_years_mixed.rds")

# Load boundary files
const24 <- readRDS("const24_simplified.rds")
dea14 <- readRDS("dea14_simplified.rds")
lgd14 <- readRDS("lgd14_simplified.rds")

# Variable choices
var_choices <- c(
  "Unemployment Rate" = "unemployment_rate",
  "Over 65 %" = "over65_pct",
  "Owner Occupied %" = "owner_occ_pct",
  "Social Rented %" = "soc_rent_pct",
  "Private Rented %" = "priv_rent_pct",
  "No Car %" = "no_car_pct",
  "2+ Cars %" = "two_plus_car_pct",
  "Mobile (1 year) %" = "mobile_1_year_pct",
  "Catholic %" = "catholic_pct",
  "Protestant %" = "protestant_pct",
  "Presbyterian %" = "presb_pct",
  "Church of Ireland %" = "coi_pct",
  "Methodist %" = "meth_pct",
  "Other Christian %" = "other_chr_pct",
  "Other Religion %" = "other_rel_pct",
  "No Religion %" = "no_relig_pct",
  "Religion Not Stated %" = "not_stated_relig_pct",
  "No Religion + Not Stated %" = "no_relig_plus_relig_not_stated_pct",
  "Catholic-Protestant Balance %" = "cath_prot_balance_pct"
)

ui <- fluidPage(
  titlePanel("Census Data Explorer (All Years)"),
  
  sidebarLayout(
    sidebarPanel(
      # Grid type selection
      selectInput("grid_type", "Grid Square Type:",
                  choices = c("1km" = "1km", 
                              "100m" = "100m", 
                              "Mixed" = "mixed"),
                  selected = "1km"),
      
      # Map type
      radioButtons("map_type", "Map Type:",
                   choices = c("Single Year" = "single", 
                               "Change Between Years" = "change"),
                   selected = "single"),
      
      # Year selection (conditional)
      conditionalPanel(
        condition = "input.map_type == 'single'",
        selectInput("year", "Select Year:",
                    choices = c("1971" = "1971", "1981" = "1981", 
                                "1991" = "1991", "2001" = "2001",
                                "2011" = "2011", "2021" = "2021"),
                    selected = "2021")
      ),
      
      # Year range for change (conditional)
      conditionalPanel(
        condition = "input.map_type == 'change'",
        selectInput("year_from", "From Year:",
                    choices = c("1971" = "1971", "1981" = "1981", 
                                "1991" = "1991", "2001" = "2001",
                                "2011" = "2011", "2021" = "2021"),
                    selected = "1971"),
        selectInput("year_to", "To Year:",
                    choices = c("1971" = "1971", "1981" = "1981", 
                                "1991" = "1991", "2001" = "2001",
                                "2011" = "2011", "2021" = "2021"),
                    selected = "2021")
      ),
      
      # Variable selection
      selectInput("variable", "Select Variable:",
                  choices = var_choices,
                  selected = "catholic_pct"),
      
      # Geographic division
      selectInput("geo_type", "Geographic Division:",
                  choices = c("SDZ 2021" = "SDZ2021_nm",
                              "DEA 2014" = "DEA2014_nm",
                              "LGD 2014" = "LGD2014_nm",
                              "Constituency 2024" = "const24_nm"),
                  selected = "const24_nm"),
      
      # Specific area (will be updated based on geo_type)
      uiOutput("area_selector"),
      
      # Transparency
      sliderInput("alpha", "Transparency:",
                  min = 0, max = 1, value = 0.7, step = 0.1),
      
      # Show boundaries
      checkboxInput("show_boundaries", "Show Geographic Boundaries", 
                    value = TRUE),
      
      # Basemap selection
      selectInput("basemap", "Basemap:",
                  choices = c(
                    "OpenStreetMap" = "OpenStreetMap",
                    "CartoDB Positron" = "CartoDB.Positron",
                    "CartoDB Dark Matter" = "CartoDB.DarkMatter",
                    "Esri World Imagery" = "Esri.WorldImagery",
                    "Esri World Street Map" = "Esri.WorldStreetMap",
                    "Stamen Terrain" = "Stamen.Terrain",
                    "Stamen Toner" = "Stamen.Toner",
                    "Stamen Toner Lite" = "Stamen.TonerLite"
                  ),
                  selected = "OpenStreetMap")
    ),
    
    mainPanel(
      leafletOutput("map", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  # Get the appropriate dataset
  selected_data <- reactive({
    switch(input$grid_type,
           "1km" = all_years_1km,
           "100m" = all_years_100m,
           "mixed" = all_years_mixed)
  })
  
  # Update area selector based on geographic division
  output$area_selector <- renderUI({
    data <- selected_data()
    areas <- sort(unique(data[[input$geo_type]]))
    areas <- areas[!is.na(areas)]
    
    selectInput("area", "Select Area:",
                choices = c("All" = "all", areas),
                selected = "all")
  })
  
  # Filter data based on selections
  filtered_data <- reactive({
    req(input$area)
    data <- selected_data()
    
    if (input$map_type == "single") {
      # Single year map
      data <- data %>% filter(year == input$year)
      
      if (input$area != "all") {
        data <- data %>% filter(.data[[input$geo_type]] == input$area)
      }
      
      # Transform to WGS84 for leaflet
      data <- st_transform(data, crs = 4326)
      
      return(data)
      
    } else {
      # Change map
      data_from <- data %>% 
        filter(year == input$year_from) %>%
        select(gridsquare, all_of(input$variable), geometry)
      
      data_to <- data %>% 
        filter(year == input$year_to) %>%
        select(gridsquare, all_of(input$variable), all_of(input$geo_type))
      
      # Calculate change
      data_change <- data_from %>%
        st_drop_geometry() %>%
        inner_join(
          data_to %>% st_drop_geometry(),
          by = "gridsquare",
          suffix = c("_from", "_to")
        ) %>%
        left_join(
          data_to %>% select(gridsquare, geometry),
          by = "gridsquare"
        ) %>%
        st_as_sf() %>%
        mutate(change = !!sym(paste0(input$variable, "_to")) - 
                 !!sym(paste0(input$variable, "_from")))
      
      if (input$area != "all") {
        data_change <- data_change %>% 
          filter(.data[[input$geo_type]] == input$area)
      }
      
      # Transform to WGS84 for leaflet
      data_change <- st_transform(data_change, crs = 4326)
      
      return(data_change)
    }
  })
  
  # Create color palette function
  get_palette <- reactive({
    data <- filtered_data()
    
    if (input$map_type == "single") {
      values <- data[[input$variable]]
      
      if (input$variable == "cath_prot_balance_pct") {
        # Diverging palette centered at 50
        colorNumeric(
          palette = c("#00008B", "#F0E68C", "#006400"),
          domain = values,
          na.color = "transparent"
        )
      } else if (input$variable == "catholic_pct") {
        colorNumeric(
          palette = c("#CCCCCC", "#006400"),
          domain = values,
          na.color = "transparent"
        )
      } else if (input$variable == "protestant_pct") {
        colorNumeric(
          palette = c("#CCCCCC", "#00008B"),
          domain = values,
          na.color = "transparent"
        )
      } else {
        colorNumeric(
          palette = "YlOrRd",
          domain = values,
          na.color = "transparent"
        )
      }
    } else {
      # Change map - diverging palette
      values <- data$change
      colorNumeric(
        palette = c("#0000FF", "#FFFFFF", "#FF0000"),
        domain = c(-max(abs(values), na.rm = TRUE), max(abs(values), na.rm = TRUE)),
        na.color = "transparent"
      )
    }
  })
  
  # Create map
  output$map <- renderLeaflet({
    req(filtered_data())
    
    data <- filtered_data()
    pal <- get_palette()
    
    # Get variable label
    var_label <- names(var_choices)[var_choices == input$variable]
    
    # Determine which column to map
    if (input$map_type == "single") {
      map_column <- input$variable
      legend_title <- var_label
    } else {
      map_column <- "change"
      legend_title <- paste("Change in", var_label)
    }
    
    # Create popup text
    if (input$map_type == "single") {
      popup_text <- paste0(
        "<strong>Grid Square:</strong> ", data$gridsquare, "<br>",
        "<strong>", input$geo_type, ":</strong> ", data[[input$geo_type]], "<br>",
        "<strong>", var_label, ":</strong> ", round(data[[input$variable]], 2)
      )
    } else {
      popup_text <- paste0(
        "<strong>Grid Square:</strong> ", data$gridsquare, "<br>",
        "<strong>", input$geo_type, ":</strong> ", data[[input$geo_type]], "<br>",
        "<strong>Change:</strong> ", round(data$change, 2)
      )
    }
    
    # Create base map
    map <- leaflet(data) %>%
      addProviderTiles(input$basemap) %>%
      addPolygons(
        fillColor = ~pal(data[[map_column]]),
        fillOpacity = input$alpha,
        color = "white",
        weight = 0.5,
        popup = popup_text,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = min(input$alpha + 0.2, 1)
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~data[[map_column]],
        title = legend_title,
        position = "topright",
        opacity = 1
      )
    
    # Add boundaries if requested
    if (input$show_boundaries) {
      boundary_data <- switch(input$geo_type,
                              "const24_nm" = const24,
                              "DEA2014_nm" = dea14,
                              "LGD2014_nm" = lgd14,
                              NULL)
      
      if (!is.null(boundary_data)) {
        # Filter boundaries if specific area selected
        if (input$area != "all") {
          name_col <- input$geo_type
          
          if (name_col %in% names(boundary_data)) {
            boundary_data <- boundary_data %>% 
              filter(.data[[name_col]] == input$area)
          }
        }
        
        # Transform boundaries to WGS84
        boundary_data <- st_transform(boundary_data, crs = 4326)
        
        map <- map %>%
          addPolygons(
            data = boundary_data,
            fillColor = "transparent",
            fillOpacity = 0,
            color = "black",
            weight = 2,
            popup = NULL
          )
      }
    }
    
    map
  })
}

shinyApp(ui = ui, server = server)