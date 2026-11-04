
# ============================================================
# INDIA WEATHER ANALYTICS PRO
# Real Historical Weather Dashboard
# R | SHINY | GGPLOT2 | DT
# ============================================================

# 1. INSTALL AND LOAD PACKAGES

packages <- c(
  "shiny", "ggplot2", "dplyr",
  "httr", "jsonlite", "DT", "lubridate"
)

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}

invisible(lapply(packages, library, character.only = TRUE))

# 2. PROJECT SETTINGS

START_DATE <- "2024-01-01"
END_DATE <- as.character(Sys.Date() - 5)
DATA_FILE <- "weather_india_data.csv"

cities <- data.frame(
  City = c(
    "Delhi", "Mumbai", "Kolkata", "Chennai",
    "Bengaluru", "Hyderabad", "Jaipur", "Lucknow",
    "Patna", "Bhopal", "Ahmedabad", "Amritsar"
  ),
  State = c(
    "Delhi", "Maharashtra", "West Bengal", "Tamil Nadu",
    "Karnataka", "Telangana", "Rajasthan", "Uttar Pradesh",
    "Bihar", "Madhya Pradesh", "Gujarat", "Punjab"
  ),
  Latitude = c(
    28.6139, 19.0760, 22.5726, 13.0827,
    12.9716, 17.3850, 26.9124, 26.8467,
    25.5941, 23.2599, 23.0225, 31.6340
  ),
  Longitude = c(
    77.2090, 72.8776, 88.3639, 80.2707,
    77.5946, 78.4867, 75.7873, 80.9462,
    85.1376, 77.4126, 72.5714, 74.8723
  )
)

required_columns <- c(
  "Date", "City", "State", "Temperature_C",
  "Humidity_pct", "Rainfall_mm",
  "WindSpeed_kmph", "Weather_Condition"
)

# 3. WEATHER CONDITION

get_condition <- function(code) {
  if (is.na(code)) return("Unknown")
  if (code == 0) return("Clear")
  if (code %in% c(1, 2, 3)) return("Cloudy")
  if (code %in% c(45, 48)) return("Fog")
  if (code %in% c(51, 53, 55, 56, 57)) return("Drizzle")
  if (code %in% c(61, 63, 65, 66, 67, 80, 81, 82)) return("Rain")
  if (code %in% c(71, 73, 75, 77, 85, 86)) return("Snow")
  if (code %in% c(95, 96, 99)) return("Thunderstorm")
  "Other"
}

# 4. DOWNLOAD WEATHER FOR ONE CITY

download_city <- function(city_row) {
  
  response <- httr::RETRY(
    "GET",
    "https://archive-api.open-meteo.com/v1/archive",
    query = list(
      latitude = city_row$Latitude,
      longitude = city_row$Longitude,
      start_date = START_DATE,
      end_date = END_DATE,
      hourly = paste(
        "temperature_2m",
        "relative_humidity_2m",
        "precipitation",
        "wind_speed_10m",
        "weather_code",
        sep = ","
      ),
      timezone = "Asia/Kolkata",
      wind_speed_unit = "kmh"
    ),
    times = 3,
    pause_min = 2,
    pause_cap = 8,
    httr::timeout(90)
  )
  
  httr::stop_for_status(response)
  
  result <- jsonlite::fromJSON(
    httr::content(response, "text", encoding = "UTF-8")
  )
  
  h <- result$hourly
  
  if (is.null(h$time) || length(h$time) == 0) {
    stop(paste("No data received for", city_row$City))
  }
  
  hourly <- data.frame(
    Date = as.Date(substr(h$time, 1, 10)),
    Temperature_C = h$temperature_2m,
    Humidity_pct = h$relative_humidity_2m,
    Rainfall_mm = h$precipitation,
    WindSpeed_kmph = h$wind_speed_10m,
    Weather_Code = h$weather_code
  )
  
  daily <- hourly %>%
    group_by(Date) %>%
    summarise(
      Temperature_C = mean(Temperature_C, na.rm = TRUE),
      Humidity_pct = mean(Humidity_pct, na.rm = TRUE),
      Rainfall_mm = sum(Rainfall_mm, na.rm = TRUE),
      WindSpeed_kmph = mean(WindSpeed_kmph, na.rm = TRUE),
      Weather_Code = {
        x <- Weather_Code[!is.na(Weather_Code)]
        if (length(x) == 0) {
          NA_real_
        } else {
          as.numeric(names(which.max(table(x))))
        }
      },
      .groups = "drop"
    )
  
  daily$City <- city_row$City
  daily$State <- city_row$State
  daily$Weather_Condition <- vapply(
    daily$Weather_Code,
    get_condition,
    character(1)
  )
  
  daily %>%
    select(all_of(required_columns))
}

# 5. DOWNLOAD ALL CITIES

download_all <- function() {
  
  result <- vector("list", nrow(cities))
  
  for (i in seq_len(nrow(cities))) {
    
    result[[i]] <- download_city(cities[i, ])
    
  }
  
  final <- bind_rows(result) %>%
    filter(
      is.finite(Temperature_C),
      is.finite(Humidity_pct),
      is.finite(Rainfall_mm),
      is.finite(WindSpeed_kmph)
    ) %>%
    arrange(Date, City)
  
  if (nrow(final) == 0) {
    stop("No valid weather data was received.")
  }
  
  write.csv(final, DATA_FILE, row.names = FALSE)
  
  final
}

# 6. CHECK AND LOAD EXISTING DATA

valid_dataset <- function(data) {
  
  all(required_columns %in% names(data)) &&
    nrow(data) > 0
}

load_dataset <- function(force_download = FALSE) {
  
  if (!force_download && file.exists(DATA_FILE)) {
    
    old_data <- tryCatch(
      read.csv(DATA_FILE, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    
    if (!is.null(old_data) && valid_dataset(old_data)) {
      old_data$Date <- as.Date(old_data$Date)
      return(old_data)
    }
  }
  
  download_all()
}

# 7. PROFESSIONAL USER INTERFACE

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background: #edf2f8;
        font-family: 'Segoe UI', Arial, sans-serif;
        color: #172b4d;
      }

      .container-fluid {
        padding-left: 25px;
        padding-right: 25px;
      }

      .hero {
        background: linear-gradient(120deg, #101d38, #075985, #087e8b);
        padding: 30px;
        border-radius: 16px;
        color: white;
        margin: 18px 0 22px 0;
        box-shadow: 0 8px 25px rgba(15,40,75,.18);
      }

      .hero h1 {
        font-size: 30px;
        font-weight: 800;
        margin-top: 8px;
        letter-spacing: .5px;
      }

      .hero p {
        color: #dbeafe;
        font-size: 14px;
      }

      .hero-tag {
        background: rgba(255,255,255,.15);
        display: inline-block;
        padding: 6px 12px;
        border-radius: 20px;
        font-size: 12px;
      }

      .side-card, .chart-card, .table-card {
        background: white;
        border-radius: 15px;
        padding: 19px;
        box-shadow: 0 4px 16px rgba(20,40,70,.06);
        margin-bottom: 19px;
      }

      .side-card h4, .chart-card h4, .table-card h4 {
        font-weight: 750;
        color: #142d4e;
        margin-top: 3px;
        margin-bottom: 15px;
      }

      .kpi-card {
        background: white;
        border-radius: 14px;
        padding: 19px;
        margin-bottom: 19px;
        box-shadow: 0 4px 16px rgba(20,40,70,.06);
        border-top: 4px solid #0284c7;
        min-height: 116px;
      }

      .kpi-title {
        color: #64748b;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: .6px;
      }

      .kpi-number {
        color: #12365a;
        font-size: 25px;
        font-weight: 800;
        margin-top: 12px;
      }

      .kpi-sub {
        color: #94a3b8;
        font-size: 11px;
      }

      .btn-primary {
        background: #087e8b;
        border-color: #087e8b;
        border-radius: 8px;
        font-weight: 700;
      }

      .form-control {
        border-radius: 8px;
        border-color: #dce5ef;
      }

      .status-line {
        border-radius: 10px;
        margin-bottom: 17px;
      }

      .footer {
        text-align: center;
        padding: 18px;
        color: #718096;
        font-size: 12px;
      }

      .control-label {
        font-weight: 650;
        color: #334e68;
      }
    "))
  ),
  
  div(
    class = "hero",
    span(class = "hero-tag", "REAL WEATHER DATA  |  INDIA"),
    h1("INDIA WEATHER ANALYTICS"),
    p("Explore temperature, rainfall, humidity and wind patterns across Indian cities."),
    p("Historical Weather Intelligence Dashboard  •  Open-Meteo API")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      div(
        class = "side-card",
        
        h4("⚙ Dashboard Controls"),
        
        selectInput(
          "state",
          "Select State",
          choices = c("All States", sort(unique(cities$State))),
          selected = "All States"
        ),
        
        selectInput(
          "city",
          "Select City",
          choices = c("All Cities", sort(cities$City)),
          selected = "All Cities"
        ),
        
        dateRangeInput(
          "dates",
          "Select Date Range",
          start = as.Date(START_DATE),
          end = as.Date(END_DATE),
          min = as.Date(START_DATE),
          max = as.Date(END_DATE),
          format = "yyyy-mm-dd"
        ),
        
        actionButton(
          "refresh",
          "↻ Refresh Weather Data",
          class = "btn-primary",
          width = "100%"
        ),
        
        br(), br(),
        
        p(
          "Tip: Change the filters to explore different weather patterns.",
          style = "color:#718096;font-size:12px;"
        )
      ),
      
      div(
        class = "side-card",
        h4("About This Project"),
        p("12 Indian Cities"),
        p("Daily Historical Records"),
        p("6 Data Visualizations"),
        p("CSV Export Available"),
        p(
          "Source: Open-Meteo",
          style = "font-size:12px;color:#087e8b;"
        )
      )
    ),
    
    mainPanel(
      width = 9,
      
      uiOutput("status"),
      
      fluidRow(
        column(3, div(
          class = "kpi-card",
          div(class = "kpi-title", "🌡 AVERAGE TEMPERATURE"),
          div(class = "kpi-number", textOutput("temp_value")),
          div(class = "kpi-sub", "Daily average")
        )),
        column(3, div(
          class = "kpi-card",
          div(class = "kpi-title", "💧 AVERAGE HUMIDITY"),
          div(class = "kpi-number", textOutput("humidity_value")),
          div(class = "kpi-sub", "Relative humidity")
        )),
        column(3, div(
          class = "kpi-card",
          div(class = "kpi-title", "🌧 TOTAL RAINFALL"),
          div(class = "kpi-number", textOutput("rain_value")),
          div(class = "kpi-sub", "Selected period")
        )),
        column(3, div(
          class = "kpi-card",
          div(class = "kpi-title", "💨 AVERAGE WIND"),
          div(class = "kpi-number", textOutput("wind_value")),
          div(class = "kpi-sub", "Wind speed")
        ))
      ),
      
      fluidRow(
        column(6, div(
          class = "chart-card",
          h4("📈 Temperature Trend"),
          plotOutput("temperature_plot", height = "290px")
        )),
        column(6, div(
          class = "chart-card",
          h4("💧 Humidity Trend"),
          plotOutput("humidity_plot", height = "290px")
        ))
      ),
      
      fluidRow(
        column(6, div(
          class = "chart-card",
          h4("🌧 Monthly Rainfall"),
          plotOutput("rainfall_plot", height = "290px")
        )),
        column(6, div(
          class = "chart-card",
          h4("💨 Wind Speed by City"),
          plotOutput("wind_plot", height = "290px")
        ))
      ),
      
      fluidRow(
        column(6, div(
          class = "chart-card",
          h4("☁ Weather Condition Distribution"),
          plotOutput("condition_plot", height = "290px")
        )),
        column(6, div(
          class = "chart-card",
          h4("🌡 Temperature vs Humidity"),
          plotOutput("scatter_plot", height = "290px")
        ))
      ),
      
      div(
        class = "table-card",
        h4("📊 Weather Data Explorer"),
        downloadButton(
          "download_data",
          "Download Filtered CSV",
          class = "btn-primary"
        ),
        br(), br(),
        DTOutput("weather_table")
      ),
      
      div(
        class = "footer",
        "INDIA WEATHER ANALYTICS PRO | Built with R, Shiny and ggplot2"
      )
    )
  )
)

# 8. SERVER LOGIC

server <- function(input, output, session) {
  
  weather_data <- reactiveVal(NULL)
  loading <- reactiveVal(FALSE)
  error_message <- reactiveVal(NULL)
  
  load_weather <- function(force_download = FALSE) {
    
    loading(TRUE)
    error_message(NULL)
    
    tryCatch({
      
      new_data <- if (force_download) {
        
        withProgress(
          message = "Downloading weather data for 12 cities",
          value = 0,
          {
            result <- vector("list", nrow(cities))
            
            for (i in seq_len(nrow(cities))) {
              incProgress(
                1 / nrow(cities),
                detail = paste(
                  "Downloading",
                  cities$City[i],
                  "(", i, "/12 )"
                )
              )
              
              result[[i]] <- download_city(cities[i, ])
            }
            
            final <- bind_rows(result) %>%
              filter(
                is.finite(Temperature_C),
                is.finite(Humidity_pct),
                is.finite(Rainfall_mm),
                is.finite(WindSpeed_kmph)
              ) %>%
              arrange(Date, City)
            
            if (nrow(final) == 0) {
              stop("No valid weather data received.")
            }
            
            write.csv(final, DATA_FILE, row.names = FALSE)
            final
          }
        )
        
      } else {
        
        load_dataset(FALSE)
        
      }
      
      weather_data(new_data)
      
    }, error = function(e) {
      
      error_message(conditionMessage(e))
      
    })
    
    loading(FALSE)
  }
  
  observeEvent(TRUE, {
    load_weather(FALSE)
  }, once = TRUE)
  
  observeEvent(input$refresh, {
    load_weather(TRUE)
  })
  
  # STATUS MESSAGE
  
  output$status <- renderUI({
    
    if (loading()) {
      return(div(
        class = "alert alert-info status-line",
        "Loading weather data. Please wait..."
      ))
    }
    
    if (!is.null(error_message())) {
      return(div(
        class = "alert alert-danger status-line",
        paste("Data loading error:", error_message())
      ))
    }
    
    if (is.null(weather_data())) {
      return(div(
        class = "alert alert-warning status-line",
        "Waiting for weather data..."
      ))
    }
    
    div(
      class = "alert alert-success status-line",
      paste(
        "✓ Dashboard ready |",
        format(nrow(weather_data()), big.mark = ","),
        "real daily records loaded."
      )
    )
  })
  
  # FILTERED DATA
  
  filtered_data <- reactive({
    
    if (is.null(weather_data())) {
      return(data.frame())
    }
    
    data <- weather_data()
    
    if (!is.null(input$state) && input$state != "All States") {
      data <- data %>% filter(State == input$state)
    }
    
    if (!is.null(input$city) && input$city != "All Cities") {
      data <- data %>% filter(City == input$city)
    }
    
    if (!is.null(input$dates) && length(input$dates) == 2) {
      data <- data %>%
        filter(
          Date >= as.Date(input$dates[1]),
          Date <= as.Date(input$dates[2])
        )
    }
    
    data
  })
  
  # KPI CARDS
  
  output$temp_value <- renderText({
    d <- filtered_data()
    if (nrow(d) == 0) return("N/A")
    paste0(round(mean(d$Temperature_C), 1), " °C")
  })
  
  output$humidity_value <- renderText({
    d <- filtered_data()
    if (nrow(d) == 0) return("N/A")
    paste0(round(mean(d$Humidity_pct), 1), " %")
  })
  
  output$rain_value <- renderText({
    d <- filtered_data()
    if (nrow(d) == 0) return("N/A")
    paste0(round(sum(d$Rainfall_mm), 1), " mm")
  })
  
  output$wind_value <- renderText({
    d <- filtered_data()
    if (nrow(d) == 0) return("N/A")
    paste0(round(mean(d$WindSpeed_kmph), 1), " km/h")
  })
  
  # EMPTY CHART HELPER
  
  empty_chart <- function(message) {
    ggplot() +
      annotate(
        "text",
        x = 1,
        y = 1,
        label = message,
        size = 5,
        colour = "#64748b"
      ) +
      xlim(0, 2) +
      ylim(0, 2) +
      theme_void()
  }
  
  # TEMPERATURE TREND
  
  output$temperature_plot <- renderPlot({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(empty_chart("No data for selected filters"))
    }
    
    ggplot(
      d,
      aes(
        x = Date,
        y = Temperature_C,
        colour = City,
        group = City
      )
    ) +
      geom_line(linewidth = 0.65, alpha = 0.85) +
      scale_color_brewer(palette = "Set2") +
      labs(x = NULL, y = "Temperature (°C)") +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  # HUMIDITY TREND
  
  output$humidity_plot <- renderPlot({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(empty_chart("No data for selected filters"))
    }
    
    ggplot(
      d,
      aes(
        x = Date,
        y = Humidity_pct,
        colour = City,
        group = City
      )
    ) +
      geom_line(linewidth = 0.65, alpha = 0.85) +
      scale_color_brewer(palette = "Dark2") +
      labs(x = NULL, y = "Humidity (%)") +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  # MONTHLY RAINFALL
  
  output$rainfall_plot <- renderPlot({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(empty_chart("No data for selected filters"))
    }
    
    monthly <- d %>%
      mutate(Month = floor_date(Date, "month")) %>%
      group_by(Month) %>%
      summarise(
        Rainfall = sum(Rainfall_mm),
        .groups = "drop"
      )
    
    ggplot(monthly, aes(x = Month, y = Rainfall)) +
      geom_col(fill = "#0284c7", width = 25) +
      geom_line(
        colour = "#0f172a",
        linewidth = 0.7,
        group = 1
      ) +
      labs(x = NULL, y = "Rainfall (mm)") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })
  
  # WIND SPEED
  
  output$wind_plot <- renderPlot({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(empty_chart("No data for selected filters"))
    }
    
    wind <- d %>%
      group_by(City) %>%
      summarise(
        Wind = mean(WindSpeed_kmph),
        .groups = "drop"
      ) %>%
      arrange(Wind)
    
    ggplot(
      wind,
      aes(x = reorder(City, Wind), y = Wind)
    ) +
      geom_col(fill = "#0d9488", width = 0.7) +
      coord_flip() +
      labs(x = NULL, y = "Wind Speed (km/h)") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })
  
  # WEATHER CONDITION DISTRIBUTION
  
  output$condition_plot <- renderPlot({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(empty_chart("No data for selected filters"))
    }
    
    conditions <- d %>%
      count(Weather_Condition, name = "Records")
    
    ggplot(
      conditions,
      aes(
        x = 2,
        y = Records,
        fill = Weather_Condition
      )
    ) +
      geom_col(width = 1, colour = "white") +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +
      scale_fill_brewer(palette = "Set2") +
      labs(fill = "Condition") +
      theme_void() +
      theme(legend.position = "bottom")
  })
  
  # TEMPERATURE VS HUMIDITY
  
  output$scatter_plot <- renderPlot({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(empty_chart("No data for selected filters"))
    }
    
    ggplot(
      d,
      aes(
        x = Temperature_C,
        y = Humidity_pct,
        colour = City
      )
    ) +
      geom_point(alpha = 0.5, size = 1.7) +
      scale_color_brewer(palette = "Set1") +
      labs(
        x = "Temperature (°C)",
        y = "Humidity (%)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  # DATA TABLE
  
  output$weather_table <- renderDT({
    
    d <- filtered_data()
    
    datatable(
      d,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  # DOWNLOAD FILTERED DATA
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste0("India_Weather_Data_", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
    }
  )
}

# 9. RUN APPLICATION

shinyApp(ui = ui, server = server)