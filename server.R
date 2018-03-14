library(shiny,quietly = T)
library(shinyjs, quietly = T)
library(dplyr,quietly = T)
library(ggplot2,quietly = T)
library(data.table,quietly = T)

source("helper.R")
load("./data/map.RData")

shinyServer(function(input, output) {
  withProgress(load("./data/CRANlog_cleaned_month.RData"),value=99,message="Loading 20,086,138 rows of data ... This takes 11 seconds! Please wait while it's loading.")
  options(warn = -1)
  
  react.plot <- eventReactive(input$go,{
    # withBusyIndicatorServer("go",{
    dat.p <<- subset(dat,package %in% input$package)
    dat.ts <<- aggregate(times~package+month,data=dat.p,sum)
    switch(input$plot_type,
    "Downloads vs Time (monthly)" = {
      if (input$smooth){
      DTmonthly <<- ggplot(dat.ts, aes(month, times, color = package)) + stat_smooth(se=FALSE, span=input$smooth.span) +
        geom_line() + xlab("Date") + scale_y_continuous(name="Number of downloads", labels = scales::comma)
      } else {
      DTmonthly <<- ggplot(dat.ts, aes(month, times, color = package)) + 
        geom_line() + xlab("Date") + scale_y_continuous(name="Number of downloads", labels = scales::comma)
      }
      DTmonthly},
    "Downloads vs Time (cumulative)" = {
      d = dat.ts %>%
        group_by(package) %>%
        transmute(count=cumsum(times), month=month) 
      DTcumu <<- ggplot(d, aes(month, count, color = package)) + geom_line() +
        xlab("Date") + scale_y_continuous(name="Number of downloads", labels = scales::comma)
      DTcumu},
    "Map (cumulative)" = {
      shiny::validate(
        need(length(input$package)==1, "Select one package for Map (cumulative) plot.")
      )
      withProgress(message = "Working really hard to plot ... ", value = 99,{
        if (is.null(dat.p)){dat.p <<- subset(dat,package %in% input$package)}
        # ma <- broom::tidy(rworldmap::getMap()[-which(rworldmap::getMap()$ADMIN=="Antarctica"),])
        # ma$id <- countrycode::countrycode(ma$id,"country.name","iso2c")
        ma <- plyr::join(ma,  aggregate(times ~ country, data = dat.p, sum), by ="country")
        if (!complete.cases(ma)){ ma[!complete.cases(ma),]$times <- 0 }
        DTmap <<- ggplot() + geom_polygon(data = ma, aes(x = long, y = lat, group = group, fill = log10(times)),
                                          colour = "black", size = 0.3) +
          ggtitle(paste("Map of the Cumulative Downloads of",input$package)) +
          annotate("text",x=-100,y=-75,label=paste("Total download since",months(dat.p$month[1]),year(dat.p$month[1]),
                                                   "is",sum(dat.p$times))) + coord_fixed(ratio = 1.4)
        DTmap
        })
        },
    "Map (dominance)" = {
      shiny::validate(
        need(length(input$package)==2, "Select two packages for Map (dominance) plot.")
      )
      withProgress(message = "Plotting really hard ... ", value = 99, {
        if (is.null(dat.p)){dat.p <<- subset(dat,package %in% input$package)}
        dat.p1 <- subset(dat.p,package==input$package[1] & month>=input$times[1] & month<=input$times[2])
        dat.p2 <- subset(dat.p,package==input$package[2] & month>=input$times[1] & month<=input$times[2])
        colnames(dat.p1)[5] <- "times1"
        colnames(dat.p2)[5] <- "times2"
        domination <- plyr::join(aggregate(times1 ~ country, data = dat.p1, sum),
             aggregate(times2 ~ country, data = dat.p2, sum), by ="country", type = "full")
        domination[is.na(domination)] <- 0
        
        for (i in 1:nrow(domination)){
          if(domination$times1[i]+domination$times2[i]==0){
            domination$dominance[i]<-"None"
          } else {if (domination$times1[i]>=domination$times2[i]){
            domination$dominance[i]<-paste(input$package[1])
          } else {
            domination$dominance[i]<-paste(input$package[2])
          }}
        }
        ma <- plyr::join(ma, domination, by ="country")
        if (!complete.cases(ma)){ ma[!complete.cases(ma),]$times <- 0 }

        DTmap <<- ggplot() +
          geom_polygon(data = ma, aes(x = long, y = lat, group = group, fill = dominance), 
                       colour = "black", size = 0.3) +
          ggtitle(paste("Dominance Map of",input$package[1],"and",input$package[2])) + coord_fixed(ratio = 1.4)
        DTmap
        }
        )
    })
  })
  
  output$plot1 <- renderPlot({
    react.plot()
  })
  
  output$timeslide <- renderUI({
    if(input$plot_type=="Map (dominance)"){
      range.time <-  range(subset(dat,package %in% input$package)$month)
      sliderInput("times","Time Selection",min=range.time[1],max=range.time[2],
                  value=range.time,timeFormat="%b %Y")
    }
  })
  
  output$smooth.spanning <- renderUI({
    if (is.null(input$smooth)) {
    } else if(input$smooth & input$plot_type=="Downloads vs Time (monthly)"){
      sliderInput("smooth.span","Smoothness Level",min=0.2,max=1.0,value=0.5)
    }
  })
  
  output$smoothing <- renderUI({
    if (input$plot_type=="Downloads vs Time (monthly)")
    checkboxInput("smooth", "With smoothing?", value = FALSE)
  })
  
  
  react.top5.des <- eventReactive(input$go,
                              {if (input$plot_type=="Map (cumulative)"){
                                print("The top ten countries are:")
                              } else {NULL}})
  
  output$top5.des <- renderText({
    react.top5.des()
  })
  
  react.top5 <- eventReactive(input$go,{
    shiny::validate(
      need(length(input$package)==1, "Select one package for Map (cumulative) plot.")
    )
    dat.p$country.name <- countrycode::countrycode(dat.p$country.name, "country.name","country.name")
      dat.country <- aggregate(times~country.name,data=dat.p,FUN = sum)
      dat.country[order(dat.country$times,decreasing = T),][1:10,]
    } else {NULL}
  })
  
  output$top5 <- renderTable({
    react.top5()
  })
  
  
  output$download_data <- renderUI({
    downloadLink('downloaddata', "Download Selected Data")
  })
  
  output$downloaddata <- downloadHandler(
    # specify the file name
    filename = function() { 
      paste(input$package, 'csv', sep='.')
    },
    #open the device, create the plot, close the device
    content = function(file) {
      write.csv(dat.p,file)
    }
  )
  
  output$download_plot <- renderUI({
    downloadLink('downloadplot', "Download Plot")
  })
  
  output$downloadplot <- downloadHandler(
    # specify the file name
    filename = function() { 
      paste(input$package, 'pdf', sep='.')
    },
    #open the device, create the plot, close the device
    content = function(file) {
      pdf(file,width = 12)
      if(input$plot_type == "Downloads vs Time (monthly)"){
        print(DTmonthly)
      } else if (input$plot_type == "Downloads vs Time (cumulative)"){
        print(DTcumu)
      } else {
        print(DTmap)
      }
      dev.off()
    }     
  )
})