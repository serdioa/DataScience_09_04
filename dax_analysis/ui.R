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

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("DAX Performance Index"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
       selectInput("symbol",
                   "Index / Stock:",
                   choices = symbols),
       checkboxInput("showPrice",
                     "Show Index/Stock Price",
                     TRUE),
       sliderInput("days",
                   "Time frame, days:",
                   min = 10,
                   max = 200,
                   value = 50),
       selectInput("trendType",
                   "Trend Indicator:",
                   choices = trendTypes),
       checkboxInput("showTrend",
                     "Show Trend Indicator",
                     TRUE)
    ),

    # Show a plot of the generated distribution
    mainPanel(
       plotlyOutput("distPlot"),
       textOutput("selectedShowPrice"),
       textOutput("selectedTrendType"),
       textOutput("selectedShowTrend")
    )
  )
))
