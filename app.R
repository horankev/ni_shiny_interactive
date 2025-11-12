library(shiny)
library(tmap)
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
                    value = TRUE)
    ),
    
    mainPanel(
      tmapOutput("map", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  
  tmap_mode("view")
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
      
      return(data_change)
    }
  })
  
  # Create map
  output$map <- renderTmap({
    req(filtered_data())
    
    # Get variable label
    var_label <- names(var_choices)[var_choices == input$variable]
    
    if (input$map_type == "single") {
      # Single year map
      
      # Use custom diverging scale for Catholic-Protestant Balance
      if (input$variable == "cath_prot_balance_pct") {
        fill_scale <- tm_scale_continuous(
          values = c("lightsalmon1", "khaki", "turquoise3"),
          midpoint = 50
        )
        }
      else if (input$variable == "catholic_pct") {
        fill_scale <- tm_scale_continuous(
          values = c("gray80", "darkgreen")
        )
      } 
      else if (input$variable == "protestant_pct") {
        fill_scale <- tm_scale_continuous(
          values = c("gray80", "darkblue")
        )
      } 
      else {
        fill_scale <- tm_scale_continuous(values = "brewer.yl_or_rd")
      }
      
      map <- tm_shape(filtered_data()) +
        tm_fill(
          fill = input$variable,
          fill.scale = fill_scale,
          fill.legend = tm_legend(title = var_label, position = tm_pos_in("left", "top")),
          fill_alpha = input$alpha,
          popup.vars = c("gridsquare", input$geo_type, input$variable)
        )
      
      title_text <- paste0(input$year, ": ", var_label)
      
    } else {
      # Change map
      map <- tm_shape(filtered_data()) +
        tm_fill(
          fill = "change",
          fill.scale = tm_scale_continuous(
            values = c("blue", "white", "red"),
            midpoint = 0
          ),
          fill.legend = tm_legend(title = paste("Change in", var_label), position = tm_pos_in("left", "top")),
          fill_alpha = input$alpha,
          popup.vars = c("gridsquare", input$geo_type, "change")
        )
      
      title_text <- paste0("Change ", input$year_from, " to ", input$year_to, 
                           ": ", var_label)
    }
    
    
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
          # Get the name column from boundary data (same as geo_type)
          name_col <- input$geo_type
          
          if (name_col %in% names(boundary_data)) {
            boundary_data <- boundary_data %>% 
              filter(.data[[name_col]] == input$area)
          }
        }
        
        map <- map +
          tm_shape(boundary_data) +
          tm_borders(col = "black", lwd = 2)
      }
    }
    
    map <- map +
      tm_title(title_text) +
      tm_layout(legend.reverse = TRUE)
    
    map
  })
}

shinyApp(ui = ui, server = server)