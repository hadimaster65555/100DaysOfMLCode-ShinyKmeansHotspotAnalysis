#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(clustertend)
library(ggmap)
library(maps)
library(ggplot2)
library(cluster)
library(stats)
library(factoextra)
library(RCurl)

# Define server logic
shinyServer(function(input, output) {
   
  # Taking dataset from internet
  allData <- reactive({
    repeat {
      result <- tryCatch({
        peringatan = ""
        download = getURL("https://raw.githubusercontent.com/hadimaster65555/100DaysOfMLCode-ShinyKmeansHotspotAnalysis/master/Data-Riau.csv")
        df = read.csv2(text = download)
      }, warning = function(w) {
        peringatan = "warning"
        print(peringatan)
      }, error = function(e) {
        peringatan = "error"
        print(peringatan)
      })
      if (result != "warning" && result != "error") {
        print("File successfully loaded")
        break
      }
    }
    print(result)
    result
  })
  
  selectedDataSet <- reactive({
    allData()[allData()$TAHUN == input$tahun & allData()$BULAN == input$bulan, ]
  })
  
  # K-means
  clusters <- reactive({
      kmeans(x = selectedDataSet()[,2:3], 
             centers = input$center,
             iter.max = input$maxIter,
             algorithm = input$algorithm)
  })
  
  # Clustering Analysis
  analysisResult <- reactive({
    eclust(x = selectedDataSet()[,2:3], k = input$center , FUNcluster = "kmeans", hc_metric = "euclidean")
  })
  
  # Hopkins
  hopkinsResult <- reactive({
    set.seed(123)
    hopkins(data = selectedDataSet()[,2:3], n = nrow(selectedDataSet())-1)$H
  })
  
  # MonthData
  monthData <- reactive({
    c("January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December")
  })
  
  # Render Month
  output$month <- renderUI({
    print(monthData()[as.numeric(input$bulan)])
    strong(monthData()[as.numeric(input$bulan)])
  })
  
  # Get map from Google Map (Require internet connection)
  mapFromGoogle <- reactive({
    repeat {
      result <- tryCatch({
        peringatan = ""
        peta = get_map(location = "Riau", zoom = 7)
      }, warning = function(w) {
        peringatan = "warning"
        print(peringatan)
      }, error = function(e) {
        peringatan = "error"
        print(peringatan)
      })
      if (result != "warning" && result != "error") {
        print("map successfully loaded")
        break
      }
    }
    print(result)
    result
  })
  
  output$mapTable <- renderTable({
    data.frame(Summary = c(
                "Number of Cluster",
                "Number of Data Points",
                "Hopkins",
                "Between SS/Total SS",
                "Cluster Avg Width"
              ), Value = c(
                input$center,
                nrow(selectedDataSet()),
                hopkinsResult(),
                clusters()$betweens/clusters()$totss,
                analysisResult()$silinfo$avg.width
              )
              )
  })
  
  # Plot map and its cluster
    output$distPlot <- renderPlot({
      print(paste0('Render plot for ', input$bulan,', ', input$tahun))
      df = selectedDataSet()
      df$cluster = factor(clusters()$cluster)
      ggmap(mapFromGoogle()) +
        geom_point(data = df,aes(
                                 x = LONGITUDE,
                                 y = LATITUDE,
                                 size = BRIGHTNESS,
                                 color = cluster
                                )
                   )
    })
    
    # Optimal cluster plot
    output$wssPlot <- renderPlot({
      fviz_nbclust(x = selectedDataSet()[,2:3], 
                   FUNcluster = kmeans,
                   method = "wss")
    })
    
    output$silPlot <- renderPlot({
      fviz_nbclust(x = selectedDataSet()[,2:3],
                   FUNcluster = kmeans,
                   method = "silhouette")
    })

    output$gapPlot <- renderPlot({
      set.seed(111)
      fviz_nbclust(x = selectedDataSet()[,2:3],
                   FUNcluster = kmeans,
                   method = "gap_stat",
                   nstart = 25,
                   nboot = 50)
    })
    
    # Panel 2
    # Generate Dataframe For Hotspot Table By Year
    DataForHotspotTableByYear <- reactive({
      year = c(2015,2016,2017,2018)
      numHotspot = c()
      for (i in 1:length(year)) {
        numHotspot[i] = nrow(allData()[allData()$TAHUN == year[i], ])
      }
      data.frame(Year = as.integer(year),
                 N.Hotspot = numHotspot
                 )
    })
    
    output$hotspotTableByYear <- renderTable({
      DataForHotspotTableByYear()
    })
    
    
    # Render Plot for Hotspot By Year
    output$hotspotPlotByYear <- renderPlot({
      hotspotDf = DataForHotspotTableByYear()
      hotspotDf$Year = as.character(hotspotDf$Year)
      ggplot(hotspotDf, aes(x = Year, y = N.Hotspot, fill = N.Hotspot)) + geom_col()
    })
    
    # Render Year Using loop
    lapply(1:4, function(i) {
      output[[paste0('yearHotspot',i)]] <- renderUI({
        strong(paste0("Year ", 2014+i))
      })
    })
    
    # Render Table Using Loop
    lapply(1:4, function(i) {
      output[[paste0('hotspotTableByMonth',i)]] <- renderTable({
        nSpot = c()
        temp = allData()[allData()$TAHUN == 2014+i, ]
        for (j in 1:length(unique(temp$BULAN))) {
          nSpot[j] = nrow(temp[temp$BULAN == j,])
        }
        df = data.frame(Month = monthData()[1:j], N.Hotspots = nSpot)
      })
    })
    
    # Render Plot Using Loop
    lapply(1:4, function(i) {
      output[[paste0('plotTitikPanas',i)]] <- renderPlot({
        nSpot = c()
        temp = allData()[allData()$TAHUN == 2014+i, ]
        for (j in 1:length(unique(temp$BULAN))) {
          nSpot[j] = nrow(temp[temp$BULAN == j,])
        }
        df = data.frame(Month = factor(monthData()[1:j], levels = month.name[1:j]), N.Hotspots = nSpot)
        print(df$Month)
        ggplot(df, aes(x = Month, y = N.Hotspots, fill = N.Hotspots)) + geom_col()
      })
    })
    
})