# Load required libraries
library(shiny)
library(ggplot2)
library(lubridate)
library(shinyWidgets)
library(gt)

# Read the inflation data from a csv file
inflation_data <- read.csv("https://raw.githubusercontent.com/canovasjm/inflacion/main/inflation_data.csv")

# Define the UI
ui <- fluidPage(
  
  # App title
  titlePanel("Inflación acumulada Argentina"),
  
  # Background color for verbatimTextOutput()
  tags$head(
    tags$style(HTML("
      /* this will affect all the pre elements */
      pre {
        color: black;
        background-color: #ffffff;
        font-weight: bolder;
        font-size: 1.5em;
      }"))
  ),
  
  # Sidebar with inputs for start date and end date
  sidebarLayout(
    sidebarPanel(
      # Select start date
      airDatepickerInput("start_date",
                         label = "Mes inicio",
                         value = "2017-01-01",
                         minDate = min(as.Date(inflation_data$date)),
                         maxDate = max(as.Date(inflation_data$date)),
                         view = "years", # Edit what the popup calendar shows when it opens
                         minView = "months", # Make it not possible to go down to a "days" view and pick the wrong date
                         dateFormat = "yyyy-MM",
                         language = "es"
      ),
      
      # Select end date
      airDatepickerInput("end_date",
                         label = "Mes fin",
                         value = max(as.Date(inflation_data$date)),
                         minDate = min(as.Date(inflation_data$date)),
                         maxDate = max(as.Date(inflation_data$date)),
                         view = "months", # Edit what the popup calendar shows when it opens
                         minView = "months", # Make it not possible to go down to a "days" view and pick the wrong date
                         dateFormat = "yyyy-MM",
                         language = "es"
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
    
    # Display plots and table
    mainPanel(
      h4("Inflación - Gráficos"),
      fluidRow(
        column(
          width = 6,
          downloadButton("download_inflation_monthly", "Descargar gráfico inflación mensual"),
          plotOutput("inflation_monthly_plot") # Monthly inflation plot
        ),
        column(
          width = 6,
          downloadButton("download_inflation_cumulative", "Descargar gráfico inflación acumulada"),
          plotOutput("inflation_cumulative_plot") # Cumulative inflation plot
        )
      ),
      
      hr(), # Line break
      
      h4("Inflación - Valores"),
      downloadButton('download_csv',"Descargar datos"),
      tableOutput("inflation_table")
    )
  )
)


# Define the server
server <- function(input, output) {
  
  ###########################
  # Reactive expressions for data wrangling
  ###########################
  
  # Reactive expression to filter data
  filter_data <- reactive({
    start_date <- as.Date(paste0(input$start_date, "-01"))
    end_date <- as.Date(paste0(input$end_date, "-01"))
    
    df <- inflation_data[inflation_data$date >= start_date & inflation_data$date <= end_date, ]
    df$month_year <- format(as.Date(df$date), "%Y-%m")
    
    return(df)
  })
  
  # Reactive expression to transform data
  transform_data <- reactive({
    df <- filter_data()
    df$cumulative_inflation <- cumprod(1 + (as.numeric(df$inflation) / 100)) - 1
    
    return(df)
  })
  
  # Reactive expression to calculate cumulative inflation
  cumulative_inflation <- reactive({
    filtered_data <- filter_data()
    if (nrow(filtered_data) == 0) return(NA)
    
    inflation_rates <- 1 + (as.numeric(filtered_data$inflation) / 100)
    cumulative_inflation <- prod(inflation_rates) - 1
    
    return(cumulative_inflation)
  })
  
  
  ###########################
  # Render cumulative inflation value
  ###########################
  output$cumulative_inflation <- renderText({
    paste0(round(cumulative_inflation() * 100, 2), "%")
  })
  
  
  ###########################
  # Monthly inflation plot
  ###########################
  
  # Define function
  make_inflation_monthly <- function(data) {
    ggplot(data, aes(x = month_year, y = inflation)) +
      geom_col(linewidth = 1, group = 1) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(
        #title = "Cumulative Inflation Over Time",
        x = "Año-mes",
        y = "Inflación mensual"
      )
  }
  
  # Render plot
  output$inflation_monthly_plot <- renderPlot({
    filtered_data <- filter_data()
    make_inflation_monthly(filtered_data)
  })
  
  # Download handler
  output$download_inflation_monthly <- downloadHandler(
    filename = function() {
      paste0("inflacion_mensual_grafico_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      filtered_data <- filter_data()
      ggsave(
        file,
        plot = make_inflation_monthly(filtered_data),
        width = 8,
        height = 5,
        dpi = 300
      )
    }
  )
  
  
  ###########################
  # Cumulative inflation plot
  ###########################
  
  # Define function
  make_inflation_cumulative <- function(data) {
    ggplot(data, aes(x = month_year, y = cumulative_inflation)) +
      geom_line(linewidth = 1, group = 1) +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(
        #title = "Cumulative Inflation Over Time",
        x = "Año-mes",
        y = "Inflación acumulada"
      )
  }
  
  # Render plot
  output$inflation_cumulative_plot <- renderPlot({
    transformed_data <- transform_data() 
    make_inflation_cumulative(transformed_data)
  })
  
  # Download handler
  output$download_inflation_cumulative <- downloadHandler(
    filename = function() {
      paste0("inflacion_acumulada_grafico_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      transformed_data <- transform_data()
      ggsave(
        file,
        plot = make_inflation_cumulative(transformed_data),
        width = 8,
        height = 5,
        dpi = 300
      )
    }
  )

  
  ###########################
  # Table
  ###########################
  
  # Render table
  output$inflation_table <- render_gt({
    transformed_data <- transform_data()
    
    data.frame(
      Mes = format(as.Date(transformed_data$date), "%Y-%m"),
      Inflacion_mensual = transformed_data$inflation,
      Inflacion_acumulada = (transformed_data$cumulative_inflation) * 100
    ) %>%
      gt() %>%
      fmt_percent(columns = c("Inflacion_mensual", "Inflacion_acumulada"), decimals = 2, scale_values = FALSE) %>% # Format numeric columns
      cols_align(align = "left", columns = Mes) %>% 
      cols_label(Inflacion_mensual = "Inflacion mensual", Inflacion_acumulada = "Inflacion acumulada") %>% 
      tab_options(table.width = pct(50))
  })
  
  
  # Download handler
  output$download_csv <- downloadHandler(
    
    filename = function() {
      paste("inflacion_datos_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    
    content = function(file) {
      transformed_data <- transform_data()
      export_data <- data.frame(
        mes = format(as.Date(transformed_data$date), "%Y-%m"),
        inflacion_mensual = transformed_data$inflation,
        inflacion_acumulada = (transformed_data$cumulative_inflation) * 100
      )
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
}

# Run the app
shinyApp(ui = ui, server = server)