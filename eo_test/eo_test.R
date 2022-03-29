library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(tidyquant)
library(rvest)
library(readxl)
library(timetk)
library(plotly)
library(feasts)
library(tsibble)
library(ggHoriPlot)
library(ggplot2)
library(ggthemes)


# Import top 30 cryptocurrencies symbols downloaded from Yahoo Finance
top30 <- read_xlsx("data/top30coins.xlsx")

symbols <- top30 %>%
  select(`Symbol`)

top30symbol <- as.vector(symbols$Symbol)
from_date = "2020-01-01"
to_date = Sys.Date()
period_type = "days"  # daily prices chosen

prices <- tq_get(top30symbol, 
                 get  = "stock.prices",
                 from = "2020-01-01",
                 to   = "2022-03-29",
                 complete_cases = F) %>%
  select(symbol,date,adjusted) 

# Define UI for application
ui <- fluidPage (
  navbarPage("ShinyCoin",
             tabPanel("Summary"),
             #exploratory Analysis (Yiou)
             navbarMenu("Exploratory",
                        
                        tabPanel("Horizon Graph",
                                 sidebarLayout(
                                   sidebarPanel(
                                   dateRangeInput(inputId = "date",
                                                label = "Date range",
                                                start = "2020-01-01",
                                                end = Sys.Date(),
                                                max = Sys.Date()),
                                 submitButton("View")
                                 ),
                        mainPanel(plotOutput("hori"))
                        )
                        ),      
                          # Sidebar 
                          tabPanel("Anomaly Detection",
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
                                                      selected = 4
                                         ),
                                         
                            ),
                            
                            # Plot results
                            mainPanel(
                              plotlyOutput("anom",height=800)
                            )
                          )
                        
                        )
             ),
             
             #Statistical Analysis (BL)
             navbarMenu("Statistical",
                        
                        tabPanel("Time Series Decomposition",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "coins",
                                                 label = "Select a coin:",
                                                 choices = c(top30symbol),
                                                 selected = ""),
                                     dateRangeInput(inputId = "date", 
                                                    label = "Date range",
                                                    start = "2020-01-01",
                                                    end = Sys.Date(),
                                                    max = Sys.Date()),
                                     submitButton("View")),
                                   mainPanel(plotlyOutput("tsstl"))
                                 )
                        ), 
                        
                        tabPanel("Seasonality Diagnostics",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "coins",
                                                 label = "Select a coin:",
                                                 choices = c(top30symbol),
                                                 selected = ""),
                                     dateRangeInput(inputId = "date", 
                                                    label = "Date range",
                                                    start = "2020-01-01",
                                                    end = Sys.Date(),
                                                    max = Sys.Date()),
                                     submitButton("Analyse")),
                                   mainPanel(plotlyOutput("tsdiag"), plotlyOutput("tscorr"), plotOutput("tsauto"))
                                 )
                        ), 
                        tabPanel("Correlation",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "coins",
                                                 label = "Select coin:",
                                                 choices = c(top30symbol),
                                                 selected = ""),
                                     selectInput(inputId = "compare",
                                                 label = "Compare with:",
                                                 choices = c(top30symbol),
                                                 selected = ""),
                                     dateRangeInput(inputId = "date", 
                                                    label = "Date range",
                                                    start = "2020-01-01",
                                                    end = Sys.Date(),
                                                    max = Sys.Date()),
                                     submitButton("Compare")),
                                   mainPanel(plotlyOutput("tsccf"))
                                 )
                        )
             ),
             
             #Forecasting (XX)
             tabPanel("Prediction")
  )
)

# Define server logic

server <- function(input, output, session) {
  coins <- reactive(tq_get(input$coins, get ="stock.prices", from = input$date[1],
                           to = input$date[2]))
  
  #STL Decomposition
  coins_stl <- reactive ({
    coins() %>%
      group_by(symbol) %>%
      plot_stl_diagnostics(date, adjusted, .feature_set = c("observed", "season", "trend", "remainder"),.trend= "1 month")
  })
  output$tsstl <- renderPlotly(coins_stl())                
  
  #Seasonal Diagnostics
  coins_plots <- reactive ({
    coins() %>%
      group_by(symbol) %>%
      plot_seasonal_diagnostics(date, adjusted, .feature_set = c("month.lbl", "year"))
  })
  output$tsdiag <- renderPlotly(coins_plots())
  
  #Autocorrelation - timetk
  coins_corr <- reactive ({
    coins() %>%
      group_by(symbol) %>%
      plot_acf_diagnostics(date, adjusted)
  })
  output$tscorr <- renderPlotly(coins_corr())
  
  #Autocorrelation - feasts
  coins_auto <- reactive ({
    as_tsibble(coins(), key = symbol, index = date) %>%
      group_by(symbol) %>%
      ACF(adjusted,na.action = na.pass) %>%
      autoplot()
  })
  output$tsauto <- renderPlot(coins_auto())
  
  #Crossed Correlation - timetk
  coins2 <- reactive(tq_get(input$compare, get ="stock.prices", from = input$date[1],
                            to = input$date[2]))
  cp1 <- reactive ({pivot_wider(coins(), names_from = symbol)}) 
  cp2 <- reactive ({pivot_wider(coins2(), names_from = symbol)}) 
  combineall <- reactive ({cp1() %>% left_join(cp2(), by = "date")})
  
  coins_ccf <- reactive ({
    combineall() %>%
      #group_by(symbol) %>%
      plot_acf_diagnostics(date, adjusted.x, .ccf_vars = adjusted.y)
  })
  output$tsccf <- renderPlotly(coins_ccf())
  
  #horizonplot
  coinshori <- reactive(tq_get(top30symbol,get ="stock.prices", from = input$date[1],
                               to = input$date[2]))
  coins_h <- reactive({
    
    coinshori() %>% ggplot() +
      geom_horizon(aes(date,adjusted), origin = "midpoint") +
      #origin of horizon plot set as midpoint between the data range (default option)
      scale_fill_hcl(palette = 'RdBu', reverse = F) +
      facet_grid(symbol~.) +
      theme_few() +
      theme(
        panel.spacing.y=unit(0, "lines"),
        strip.text.y = element_text(size = 16, angle = 0, hjust = 0),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), 
        axis.title=element_text(size=20),
        plot.title = element_text(size=28, face = "bold"),
        plot.subtitle = element_text(size=20, face = "bold")
      )+
      scale_x_date(expand=c(0,0), 
                   date_breaks = "1 month", 
                   date_labels = "%b%Y") +
      xlab('Date') +
      ggtitle('Adjusted price of top 30 coins by marketcap')+
      guides(fill= guide_legend(title="stock price +(blue) or -(red)",
                                title.position = "top"))
  })
  output$hori <- renderPlot(coins_h(),width = 1200, height = 800)
  
  #anomaly detection - timetk
  
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
