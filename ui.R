library(shiny)
library(jsonlite)
library(shinyjs)

source("helper.R")
# get the list of all packages on CRAN
# package_names <- names(httr::content(httr::GET("http://crandb.r-pkg.org/-/desc")))
load("./data/names.RData")

shinyUI(navbarPage(
  # useShinyjs(),
  # Application title
  "Download of R Packages Worldwide",
  
  header=tags$head(includeScript("google-analytics.js"),
                   # tags$style(HTML(".shiny-notification {
                   #        height: 100px; width: 800px; position: fixed;
                   #        top: calc(50% - 50px); left: calc(50% - 400px);}")
                   tags$style(HTML("
    .shiny-output-error-validation {color: red; font-size:17px;}
                                   ")
                     )),
  theme="bootstrap.css",

  tabPanel("Plotting",{
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      br(),
      selectInput("package", 
                  label = "Packages",
                  selected = "ggplot2", # initialize the graph with a random package
                  choices = package_names,
                  multiple = TRUE),      
      selectInput("plot_type",
                  label = "Select Plot Type",
                  choices = c("","Downloads vs Time (monthly)","Downloads vs Time (cumulative)","Map (cumulative)","Map (dominance)"),
                  multiple = FALSE),
      uiOutput('timeslide'),
      uiOutput("smoothing"),
      uiOutput("smooth.spanning"),
      
      # Wrap the button in the function `withBusyIndicatorUI()`
      actionButton("go","Plot", class = "btn-primary", style = "border-radius: 10%;"),
      tags$hr(),
      uiOutput('download_data'),
      tags$hr(),
      HTML("Please contact <a href='http://t.uzh.ch/Oc' target = '_blank'>Craig Wang</a> for any feedback."),
      br(),HTML("<b>Note</b>: The connection may be unstable due to large dataset, please restart if the app crashes.")
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot1"),
      tableOutput("top5")
    )
  )
    }),
  tabPanel("News",
           fluidRow(
             column(width = 5,
                    HTML("version 0.9-1 (2018-05-11): Bug fixes, remove download plot, updated usage statistics",
                         "<br><br>",
                         "version 0.9 (2018-03-11): Fixed bug in country name matching, log10 times in dominance map, show top 10 countries, speed ups",
                         "<br><br>",
                         "version 0.8 (2017-11-29): Added smoothing, app usage, button feedback {https://github.com/daattali/advanced-shiny}",
                         "<br><br>",
                         "version 0.7 (2017-02-17): Hidden Google Analytics",
                         "<br><br>",
                         "version 0.6 (2017-01-10): New interface, new Map (Dominance) plot with time selection, added plot button",
                         "<br><br>",
                         "version 0.5 (2016-12-21): Show top 7 countries, added total downloads to Map (Cumulative) plot")
             ),
             column(width = 5,
                    HTML("<center><b> Application Usage Statistics </b></center>"),
                    img(src='usage.PNG', width=600,height=633, align = "center")
             )
           )),
  tabPanel("About",
           HTML("This application is based on the data (updated 2018-03-10) from RStudio CRAN mirror.",
                "<br><br>",
                "Inspired by David Robinson on <a href='http://varianceexplained.org/' target='_blank'>http://varianceexplained.org/</a>",
                "<br><br>","App Version 0.9, last updated: 2018-03-11","<br><br>"),
           img(src='Capture.PNG', width=550,height=260, align = "left"))
))
