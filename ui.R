#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)

# Define UI
shinyUI(navbarPage(
  # Application name
  "Hotspot Analysis ",
  # Navbar 1
  tabPanel(
    # Panel title for navbar
    "Hotspot Map",
    # Header Panel for page
    headerPanel("Hotspot K-Mean Analysis: Riau"),
    
    # Sidebar
    sidebarLayout(
      sidebarPanel(
        # Select input for Year
        selectInput(
          'tahun',
          "Year",
          choices = c(
            "2018" = 2018,
            "2017" = 2017,
            "2016" = 2016,
            "2015" = 2015
          ),
          selected = 2015
        ),
        # Select input for month
        selectInput('bulan', 
                    'Month',
                    choices = c(
                      "January" = 1,
                      "February" = 2,
                      "March" = 3,
                      "April" = 4,
                      "May" = 5,
                      "June" = 6,
                      "July" = 7,
                      "August" = 8,
                      "September" = 9,
                      "October" = 10,
                      "November" = 11,
                      "December" = 12
                    ), selected = 1),
        # Numeric input for number of cluster(s)
        numericInput('center', 'Number of group(s) or k', min = 0, value = 3),
        # Numeric input for maximum iteration
        numericInput('maxIter', 'Maximum iteration', min = 1, value = 10),
        # Select input for choosing algorithm
        selectInput(
          'algorithm',
          'Choose algorithm to use',
          choices = c("Hartigan-Wong", 
                      "Lloyd", 
                      "Forgy",
                      "MacQueen"),
          selected = "Hartigan-Wong"
        ),
        width = 3
      ),
      
      # Main panel
      mainPanel(
        # Generate summary table and ggmap map
          fluidRow(
            column(3,
                   uiOutput('month'),
                   withSpinner(tableOutput('mapTable'))
                   ),
            column(6,
                   h4("Riau Map"),
                   withSpinner(plotOutput('distPlot', width = "600", height = "600")))
            ),
          
          # Generate plot to suggest optimal cluster
          fluidRow(
            h3("Optimal Cluster Suggestion"),
            column(4,
                   h4("Using WSS Method"),
                   withSpinner(plotOutput('wssPlot'))
                   ),
            column(4,
                   h4("Using Silhouette Method"),
                   withSpinner(plotOutput('silPlot'))
                   ),
            column(4,
                   h4("Using Gap Statistics"),
                   withSpinner(plotOutput('gapPlot'))
                   )
          )
         )
        )
      ),
  
  # Navbar 2
  tabPanel(
    "Exploratory Data Analysis",
    # Layout Vertikal
    verticalLayout(
      # Panel title
      titlePanel('Exploratory Data Analysis of Hotspot Data: Riau'),
      
      # Generate summary table and barplot
      fluidRow(column(
        4,
        h4('Number of Hotspot by Year'),
        withSpinner(tableOutput('hotspotTableByYear'))
      ),
      column(8,
             withSpinner(plotOutput('hotspotPlotByYear'))
             )
      ),
      
      h2('Number of Hotspot by Month Every Year'),
      
      # Generate summary table and plot of Hotspot Data by Year
      lapply(1:4, function(i) {
        fluidRow(column(4,
                        uiOutput(paste0('yearHotspot',i)),
                        withSpinner(tableOutput(paste0('hotspotTableByMonth',i)))
                        ),
                 column(8,
                        plotOutput(paste0('plotTitikPanas',i))
                        )
                 )
        })
      )
  )
))
