## https://rstudio.github.io/shinydashboard/get_started.html ##

## Load Packages ## 
library(shiny)
library(dplyr)
library(ggplot2)
library(shinyjs)
library(shinydashboard)
library("ggtext")

## Load Source File(s) ## 
source("import_main.R")

## Build UI ## 
ui <- dashboardPage(skin="green",
                    dashboardHeader(title=""),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        
                        menuItem((HTML("A1: Pollution forecasting<br>(API Ingestion)")),
                                 tabName="Trends",
                                 icon=icon("fas fa-chart-line"))
                        
                        
                      )),
                    
                    dashboardBody(
                      tags$head(tags$style(HTML(
                        '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: center;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 300px;
        overflow: hidden;
        color: white;
      }
    '))),
                      tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Pollution in a pandemic </span>\');
      })
     ')),
                      
                      useShinyjs(),
                      
                      tabItems(
                        
                        tabItem(
                          
                          tabName="Trends",
                          
                          h4("Forecasting local pollutant levels before and after the Covid-19 pandemic"),
                          
                          fluidPage(
                            
                            plotOutput("ARIMA_pollutant"),
                            
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                                          draggable = FALSE, top = 100, left = "auto", right = 30, bottom = 400,
                                          width = 200, height = 140, background="navy", status = "primary",

                                          #selectInput("LA", "Local Authority", unique(Forecast_boroughs$Local.Authority.Name)),
                                          selectInput("PollutantAPI", "Location", unique(Forecast_boroughs$location)),
                                          selectInput("Emission", "Pollutant", NULL, selected = character(0))),
                            
                            
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                                          draggable = FALSE, top = 110, left = 650, right = "auto", bottom = 400,
                                          width = 225, height = 20, background="white", status = "primary",
                                          
                                          radioButtons("Insamplearima", "Show 'rolling window' forecasts?", c("Yes", "No"), selected = "No"))),
                          
                          
                          fluidRow(
                            valueBoxOutput("Precovid", width=3),
                            valueBoxOutput("Postcovid", width=3),
                            valueBoxOutput("Pctchange", width=2),
                            valueBoxOutput("EUlevel", width=2),
                            valueBoxOutput("MAE", width=2),
                          )))))
                        


## Build Server ## 
server <- function(input, output,session) {
  
## ObserveEvent and NA omit - Adds reactivity to the drop down menu + excludes NA choices
  
  observeEvent(input$PollutantAPI,{
    
    updateSelectInput(session,"Emission", choices=unique(na.omit(Forecast_boroughs$parameter[Forecast_boroughs$location==input$PollutantAPI])))
  })
  
  
## Value boxes ##
  
  Pollutant_forecasts = reactive({
    Output <- Forecast_boroughs %>% filter(location==input$PollutantAPI) %>% filter(parameter==input$Emission) 
    return(Output)
  })
  

  valueboxexample<-reactive({
    
    a <-  Pollutants_summarise %>%
      filter(parameter==input$Emission, location==input$PollutantAPI)
    
    return(a)
    
  })
  
  output$Precovid <-renderValueBox({
    valueBox(subtitle = tags$p("Return to pre-covid trends (ug/m3)", style = "font-size: 75%;"), round(valueboxexample()[,4],0), icon = icon("list"), color="orange")
  })
  
  output$Postcovid <-renderValueBox({
    valueBox(subtitle = tags$p("Locking in post-covid trends (ug/m3)", style = "font-size: 75%;"), round(valueboxexample()[,3],0), icon = icon("list"), color="blue")
  })
  
  output$Pctchange <-renderValueBox({
    valueBox(subtitle = tags$p("Pct difference", style = "font-size: 75%;"), round(valueboxexample()[,5],0), icon = icon("list"), color="teal")
  })
  
  output$EUlevel <-renderValueBox({
    valueBox(subtitle = tags$p("EU Reg Level (ug/m3)", style = "font-size: 75%;"), round(valueboxexample()[,7],0), icon = icon("list"), color="olive")
  })
  
  
  output$MAE <-renderValueBox({
    valueBox(subtitle = tags$p("Mean Average Error (ug/m3)", style = "font-size: 75%;"), round(valueboxexample()[,6],1), icon = icon("list"), color="aqua")
  })
  

## Scenario analysis charting ##
  
  output$ARIMA_pollutant <- renderPlot({
    
    if (input$Insamplearima=="No") { 
      
      Chart <- ggplot(Pollutant_forecasts(), aes(dateLocal)) + 
        geom_line(aes(y = value_median, colour = "Daily level (median)")) + 
        geom_line(aes(y = value_mean_roll, colour = "Four day rolling average"),size=1) +
        geom_line(aes(y = Point_High, colour = "Scenario: Pre-Covid trend")) +
        geom_line(aes(y = Point_Low, colour = "Scenario: Post-Covid trend")) +
        geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="dotdash") +
        geom_vline(xintercept = as.numeric(as.Date(Sys.Date())), linetype="dotdash") +
        xlab("Date") +
        ylab("Mg per cubic metre air") +
        theme_classic() +
        theme(legend.position="bottom") +
        scale_color_manual(values=c("grey", "red", "blue", "orange")) +
        ggtitle("Pollutant volumes and scenarios")
      
      Chart <- Chart  + geom_label(
        
        label="Covid-19 Lockdown (March 2020)",
        x=as.numeric(as.Date("2020-03-23")),
        y=layer_scales(Chart)$y$get_limits()[2],
        label.padding = unit(0.55, "lines"), # Rectangle size around label
        label.size = 0.35,
        color = "black",
        fill="#f0faf8"
      ) + labs(caption = "Daily (averaged) pollutant data recorded between 1st Jan and 14th October 2020\n EU regulatory pollutant level is shown in dashed green")
      
      if((input$Emission)=="no2" & layer_scales(Chart)$y$get_limits()[2]>40){
        Chart <- Chart + geom_hline(yintercept=40, color="green", linetype="dotdash")
        
      } else if((input$Emission)=="so2" & layer_scales(Chart)$y$get_limits()[2]>125){
        Chart <- Chart + geom_hline(yintercept=125, color="green", linetype="dotdash")
        
      } else if((input$Emission)=="pm10" & layer_scales(Chart)$y$get_limits()[2]>40){
        Chart <- Chart + geom_hline(yintercept=40, color="green", linetype="dotdash")
        
      } else if((input$Emission)=="pm25" & layer_scales(Chart)$y$get_limits()[2]>25){
        Chart <- Chart + geom_hline(yintercept=25, color="green", linetype="dotdash")
        
      } else if((input$Emission)=="o3" & layer_scales(Chart)$y$get_limits()[2]>125){
        Chart <- Chart + geom_hline(yintercept=125, color="green", linetype="dotdash")
      }
      
      Chart 
      
    } else{
      
      Chart <- ggplot(Pollutant_forecasts(), aes(dateLocal)) + 
        geom_line(aes(y = value_median, colour = "Daily level (median)")) + 
        geom_line(aes(y = `90dayrolling`, colour = "One day ahead forecast"),size=1) +
        geom_line(aes(y = Point_High, colour = "Scenario: Pre-Covid trend")) +
        geom_line(aes(y = Point_Low, colour = "Scenario: Post-Covid trend")) +
        geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype="dotdash") +
        geom_vline(xintercept = as.numeric(as.Date(Sys.Date())), linetype="dotdash") +
        xlab("Date") +
        ylab("Mg per cubic metre air") +
        theme_classic() +
        theme(legend.position="bottom") +
        scale_color_manual(values=c("grey", "black", "blue", "orange")) +
        ggtitle("Pollutant volumes and scenarios")
      
      Chart <- Chart  + geom_label(
        
        label="Covid-19 Lockdown (March 2020)",
        x=as.numeric(as.Date("2020-03-23")),
        y=layer_scales(Chart)$y$get_limits()[2],
        label.padding = unit(0.55, "lines"), # Rectangle size around label
        label.size = 0.35,
        color = "black",
        fill="#f0faf8"
      ) + labs(caption = "Daily (averaged) pollutant data recorded between 1st Jan and 28th September 2020\n EU regulatory pollutant level is shown in dashed green")
      
      if((input$Emission)=="no2" & layer_scales(Chart)$y$get_limits()[2]>40){
        Chart <- Chart + geom_hline(yintercept=40, color="green", linetype="dotdash")
        
      } else if((input$Emission)=="so2" & layer_scales(Chart)$y$get_limits()[2]>125){
        Chart <- Chart + geom_hline(yintercept=125, color="green", linetype="dotdash")
        
      } else if((input$Emission)=="pm10" & layer_scales(Chart)$y$get_limits()[2]>40){
        Chart <- Chart + geom_hline(yintercept=40, color="green", linetype="dotdash")
        
      } else if((input$Emission)=="pm25" & layer_scales(Chart)$y$get_limits()[2]>25){
        Chart <- Chart + geom_hline(yintercept=25, color="green", linetype="dotdash")
        
      } else if((input$Emission)=="o3" & layer_scales(Chart)$y$get_limits()[2]>125){
        Chart <- Chart + geom_hline(yintercept=125, color="green", linetype="dotdash")
      }
      
      Chart
      
    }
  }
  )

}
  
## Render App ## 
shinyApp(ui = ui, server = server)


