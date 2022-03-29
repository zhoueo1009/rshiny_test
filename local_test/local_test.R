
# A shiny app for monitoring cryptocurrencies and comparing stock performance
# March 2022
# Group 6
# 

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(tidyverse)
library(tidyquant)
library(readxl)
library(timetk)
library(ggHoriPlot)
library(ggplot2)
library(ggthemes)


# Import top 30 cryptocurrencies symbols downloaded from Yahoo Finance
top30 <- read_xlsx("data/top30coins.xlsx")

symbols <- top30 %>%
  select(`Symbol`)

top30symbol <- as.vector(symbols$Symbol)


prices <- tq_get(top30symbol, 
                 get  = "stock.prices",
                 from = "2020-01-01",
                 to   = Sys.Date(),
                 complete_cases = F) %>%
  select(symbol,date,adjusted)


# -----------------------------------------------------
# UI
#-------------------------------------------------------

ui <- fluidPage(#theme = shinytheme("cyborg"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(width = 3,
                 
                 # Let user pick coins
                 selectInput(
                   inputId = "coins",
                   label = h4("Coins"),
                   choices =c(top30symbol),
                   selected = "BTC-USD", 
                   multiple = F
                 ),
                 
                 # Pick time period
                 radioButtons("period", label = h4("Period"),
                              choices = list("1 month" = 1, "3 months" = 2, "6 months" = 3, "12 months" = 4, "YTD" = 5), 
                              selected = 5
                 ),
                 
    ),
    
    # Plot results
    mainPanel(
      plotlyOutput("anom",height=800)
    )
  )
)

# -----------------------------------------------------
# SERVER
#-------------------------------------------------------

server <- function(input, output,session) {
  
  # server logic based on user input
  observeEvent(c(input$period,input$coins), {
    
    prices <- prices %>%
      filter(symbol %in% input$coins)
    
    if (input$period == 1) {
      prices <- prices %>%
        filter(
          date >= today()-months(1)) }
    
    if (input$period == 2) {
      prices <- prices %>%
        filter(date >= today()-months(3)) }
    
    if (input$period == 3) {
      prices <- prices %>%
        filter(date >= today()-months(6)) }
    
    if (input$period == 5) {
      prices <- prices %>%
        filter(year(date) == year(today())) }
    
    
    # Create plot
    output$anom <- renderPlotly({
      prices %>%
        plot_anomaly_diagnostics(
                     date,
                     adjusted,
                     .facet_vars = NULL,
                     .frequency = "auto",
                     .trend = "auto",
                     .alpha = 0.05,
                     .max_anomalies = 0.2,
                     .message = TRUE,
                     .facet_ncol = 3,
                     .facet_scales = "free",
                     .facet_dir = "h",
                     .line_color = "#2c3e50",
                     .line_size = 0.5,
                     .line_type = 1,
                     .line_alpha = 1,
                     .anom_color = "#e31a1c",
                     .anom_alpha = 1,
                     .anom_size = 1.5,
                     .ribbon_fill = "grey20",
                     .ribbon_alpha = 0.2,
                     .legend_show = TRUE,
                     .title = "Anomaly Diagnostics",
                     .x_lab = "",
                     .y_lab = "price",
                     .color_lab = "Anomaly",
                     .interactive = TRUE
                   )
        
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
