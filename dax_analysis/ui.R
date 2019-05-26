#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Navbar
# https://shiny.rstudio.com/gallery/navbar-example.html
# https://shiny.rstudio.com/reference/shiny/1.0.5/navbarPage.html

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("DAX Performance Index: Trading Fast/Slow"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("symbol",
                        "Index / Stock:",
                        choices = symbols),
            sliderInput("days",
                        "Fast/Slow Indicator Periods, days:",
                        min = 10,
                        max = 200,
                        value = c(50, 150)),
            selectInput("fastIndicatorType",
                        "Fast Indicator:",
                        choices = trendTypes),
            selectInput("slowIndicatorType",
                        "Slow Indicator:",
                        choices = trendTypes)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h2("Price & Capital"),
            plotlyOutput("distPlot"),
            textOutput("selectedDaysLow"),
            textOutput("selectedDaysHigh"),
            textOutput("selectedShowPrice"),
            textOutput("selectedTrendType"),
            textOutput("selectedShowTrend"),
            h2("Trades"),
            dataTableOutput("trades")
        )
    )
))
