#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(bizdays)
library(tidyquant)
library(tidyr)
library(magrittr)
# Downloading Broadcom price using quantmod
a <- getSymbols("AVGO", from = '2020-07-01',
                to = "2020-12-01",warnings = FALSE,
                auto.assign = TRUE)

# Downloading Alexion Pharmaceuticals price using quantmod
a <- getSymbols("ALXN", from = '2020-07-01',
                to = "2020-12-01",warnings = FALSE,
                auto.assign = TRUE)

time <- index(AVGO)
start_value_AVGO <- AVGO[start(AVGO)]
start_value_AVGO_n <- as.numeric(start_value_AVGO)
end_value_AVGO <- AVGO[end(AVGO)]
end_value_AVGO_n <- as.numeric(end_value_AVGO)
return_rate_AVGO <- end_value_AVGO_n[4]/start_value_AVGO_n[4]
close_value_AVGO <- AVGO$AVGO.Close
close_value_AVGO_n <- as.numeric(close_value_AVGO)
return_rate_perday <- c()
start_value_ALXN <- ALXN[start(ALXN)]
start_value_ALXN_n <- as.numeric(start_value_ALXN)
end_value_ALXN <- ALXN[end(ALXN)]
end_value_ALXN_n <- as.numeric(end_value_ALXN)
return_rate_ALXN <- end_value_ALXN_n[4]/start_value_ALXN_n[4]
close_value_ALXN <- ALXN$ALXN.Close
close_value_ALXN_n <- as.numeric(close_value_ALXN)
time <- c(time[2:106])

ui <- fluidPage(
    
    # App title ----
    titlePanel("Portfolio two"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Slider for the number of observations to generate ----
            sliderInput("n",
                        "Select the precentage you want to buy stock AVGO:",
                        value = 100,
                        min = 0,
                        max =100)
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("plot")),
                        tabPanel("Summary", verbatimTextOutput("summary")),
                        tabPanel("Plot", plotOutput("plot2"))
            )
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot <- renderPlot({
        n <- input$n
        stock <- c("AVGO", "ALXN")
        percentage <- c(n/100, 1-n/100)
        df4 <- data.frame(stock, percentage)
        p <- ggplot(data=df4, aes(x = stock, y = percentage)) +
            geom_bar(stat="identity", fill="steelblue")+
            theme_minimal()
        p
        
    })
    output$summary<- renderPrint({
        n <- input$n
        return_amount <- c(250000)
        tempx <- 250000
        for (i in 1:105) {
            return_rate_perday[i] <- ((close_value_AVGO_n[i+1]/close_value_AVGO_n[i]) * (n/100) + (close_value_ALXN_n[i+1]/close_value_ALXN_n[i]) * (1 - n/100)) - 1
        }
        for (i in 1:105){
            return_amount[i+1] <- tempx * (return_rate_perday[i] +1 )
            tempx <- return_amount[i+1]
        }
        time2 <- index(AVGO)
        df3 <- data.frame(time2, return_amount)
        df3
        
    })
    output$plot2 <- renderPlot({
        n <- input$n
        for (i in 1:105) {
            return_rate_perday[i] <- ((close_value_AVGO_n[i+1]/close_value_AVGO_n[i]) * (n/100) + (close_value_ALXN_n[i+1]/close_value_ALXN_n[i]) * (1 - n/100)) - 1
        }
        df2 <- as.data.frame(time,return_rate_perday)
        df2 %>%
            ggplot(aes(x = time, y = return_rate_perday)) + 
            geom_hline(yintercept = 0, color = palette_light()[[1]]) +
            geom_point(size = 2, color = palette_light()[[3]]) +
            geom_line(size = 1, color = palette_light()[[3]]) + 
            labs(title = "Daily Returnrates For Portfolio2",
                 x = "", y = "Daily Return Rates", color = "") +
            theme_tq()
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

