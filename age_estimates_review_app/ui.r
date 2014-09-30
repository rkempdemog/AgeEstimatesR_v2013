library(shiny)
library("ggvis")
age5v=read.csv("age5.csv")

shinyUI(
  fluidPage(
    titlePanel("Age Estimates Compared to Forecast Age Distribution"),
    sidebarLayout(
      sidebarPanel(
        selectInput("county", "County Number:", choices=unique(sort(age5v$county)),hr()),
        selectInput( "year", "Year:", choices=c(2011, 2012, 2013), hr())
        #selectInput( "ForYear", "Forecasts Year:", choices=colnames(age5v[,8:11]), hr())
        ),
      mainPanel(
        h3("Total Population"),
        ggvisOutput("ggvis"),
        h3("Net Migration"),
        ggvisOutput("netmig"),
        h3("Deaths"),
        ggvisOutput("deaths"))
      )
    )
  )