# Load required libraries
library(shiny)
library(ggplot2)
library(lubridate)
library(shinyWidgets)

# Read the inflation data from a csv file
inflation_data <- read.csv("https://raw.githubusercontent.com/canovasjm/inflacion/main/inflation_data.csv")

# Define the UI
ui <- fluidPage(
  
  # App title
  titlePanel("Inflación acumulada Argentina"),
  
  # Sidebar with inputs for start date and end date
  sidebarLayout(
    sidebarPanel(
      # Select start date
      airDatepickerInput("start_date",
                         label = "Mes inicio",
                         value = "2017-01-01",
                         minDate = min(as.Date(inflation_data$date)),
                         maxDate = max(as.Date(inflation_data$date)),
                         view = "years", # editing what the popup calendar shows when it opens
                         minView = "months", # making it not possible to go down to a "days" view and pick the wrong date
                         dateFormat = "yyyy-MM"
      ),
      
      # Select end date
      airDatepickerInput("end_date",
                         label = "Mes fin",
                         value = max(as.Date(inflation_data$date)),
                         minDate = min(as.Date(inflation_data$date)),
                         maxDate = max(as.Date(inflation_data$date)),
                         view = "months", #editing what the popup calendar shows when it opens
                         minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
                         dateFormat = "yyyy-MM"
      ),
      
      # Display cumulative inflation
      h4(strong("Inflación acumulada del periodo")),
      verbatimTextOutput("cumulative_inflation"),
      br(),
      
      # Sources
      h4(strong("Fuente")),
      HTML("<p> INDEC, Dirección Nacional de Estadísticas de Precios, Dirección de Índices de Precios de Consumo. 
                Link: <a href=https://www.indec.gob.ar/indec/web/Nivel4-Tema-3-5-31 > https://www.indec.gob.ar/indec/web/Nivel4-Tema-3-5-31 </a></p>
            <p> Período de referencia: Diciembre 2016=100 </p>"),
      br(),
      h4(strong("Código")),
      HTML("<p> <a href=https://github.com/canovasjm/inflacion > 
                https://github.com/canovasjm/inflacion </a> </p>")
    ),
    
    # Display plot and table
    mainPanel(
      h4("Inflación acumulada a traves del tiempo"),
      plotOutput("inflation_plot"),
      hr(),
      h4("Inflación acumulada - Valores"),
      tableOutput("inflation_table")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Calculate cumulative inflation
  cumulative_inflation <- reactive({
    start_date <- as.Date(paste0(input$start_date, "-01"))
    end_date <- as.Date(paste0(input$end_date, "-01"))
    months_data <- inflation_data[inflation_data$date >= start_date & inflation_data$date <= end_date, ]
    inflation_rates <- 1 + (as.numeric(months_data$inflation) / 100)
    cumulative_inflation <- prod(inflation_rates) - 1
    return(cumulative_inflation)
  })
  
  # Display cumulative inflation
  output$cumulative_inflation <- renderText({
    paste0(round(cumulative_inflation() * 100, 2), "%")
  })
  
  # Create plot
  output$inflation_plot <- renderPlot({
    start_date <- as.Date(paste0(input$start_date, "-01"))
    end_date <- as.Date(paste0(input$end_date, "-01"))
    months_data <- inflation_data[inflation_data$date >= start_date & inflation_data$date <= end_date, ]
    months_data$month_year <- format(as.Date(months_data$date), "%Y-%m")
    months_data$cumulative_inflation <- cumprod(1 + (as.numeric(months_data$inflation) / 100)) - 1
    ggplot(months_data, aes(x = month_year, y = cumulative_inflation)) +
      geom_col(linewidth = 1, group = 1) +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(#title = "Cumulative Inflation Over Time",
           x = "Año-mes",
           y = "Inflación acumulada")
  })

  # Create table
  output$inflation_table <- renderTable({
    start_date <- as.Date(paste0(input$start_date, "-01"))
    end_date <- as.Date(paste0(input$end_date, "-01"))
    months_data <- inflation_data[inflation_data$date >= start_date & inflation_data$date <= end_date, ]
    months_data$month_year <- format(as.Date(months_data$date), "%Y-%m")
    months_data$cumulative_inflation <- (cumprod(1 + (as.numeric(months_data$inflation) / 100)) - 1) * 100
    data.frame(Mes = format(as.Date(months_data$date), "%Y-%m"), 
               Inflacion_mensual = months_data$inflation, 
               Inflacion_acumulada = months_data$cumulative_inflation)
  })
}

# Run the app
shinyApp(ui = ui, server = server)