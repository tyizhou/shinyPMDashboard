#
# This is a Shiny web to check performance marketing metrics
# Clear environment
rm(list=ls())

# Add library directory
.libPaths(c(.libPaths(),"C:/R/win-library/3.3"))

# Attach all the libraries
library(shiny)
library(RODBC)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(plotly)

# Database connection
con <- odbcDriverConnect("driver=SQL Server;server=xxx;Uid=xxx;Pwd=xxx")

# Queries
query <- "
select 
  Crunchdate as Day,
  Week,
  substring(Crunchdate,1,7) as Month,
  convert(int,substring(Crunchdate,6,2)) as monthnumber,
  Channel,
  Source,
  TargetCountry,
  sum(Clicks) as Clicks,
  sum(Conversions) as Conversions,
  sum(ConversionsAC) as ConversionsAC,
  sum(RevGross) as RevGross,
  sum(RevNet) as RevNet,
  sum(ConversionsACLinear) as ConversionsACLinear,
  sum(ConversionsLinear) as ConversionsLinear,
  sum(RevGrossLinear) as RevGrossLinear,
  sum(RevNetLinear) as RevNetLinear,
  sum(ConversionsACAssisted) as ConversionsACAssisted,
  sum(ConversionsAssisted) as ConversionsAssisted,
  sum(RevGrossAssisted) as RevGrossAssisted,
  sum(RevNetAssisted) as RevNetAssisted
from
  CDM.Report.CA_PM_Report
where
  Channel is not null
group by Crunchdate,Week,substring(Crunchdate,1,7),convert(int,substring(Crunchdate,6,2)),Channel,Source,TargetCountry
order by substring(Crunchdate,1,7),Week,Crunchdate"

# Run queries to get data from database and manipulate data
data <- sqlQuery(con, query)
data[, 8:20][is.na(data[, 8:20])] <- 0
data$Day <- as.Date(data$Day)

# Define color for plot
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
channelPalette <- c("#edf8fb","#b2e2e2","#66c2a4","#2ca25f","#006d2c","#fef0d9","#fdcc8a","#fc8d59","#e34a33","#b30000","#f1eef6","#bdc9e1","#74a9cf","#2b8cbe","#045a8d","#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")

# ui
ui <- tagList(
   navbarPage("Performance Marketing Dashboard",
############################################## Daily report dashboard ##############################################
     tabPanel("Daily Report",
       # Filter1 table
       sidebarPanel(
         width = 2,
         h4("Table Filters"),
         actionButton("update1", "Update view"),
         sliderInput("week1","Week:",
                     min = 1, max=52,
                     value = as.numeric(strftime(Sys.Date()-7, format = "%V"))),
         selectizeInput("dimensions1","Dimensions:",
                        choices = c("TargetCountry","Channel"),
                        selected = c("TargetCountry","Channel"),
                        multiple = TRUE),
         selectizeInput('country1', 'Country:', 
                        choices = unique(data$TargetCountry),
                        selected = unique(data$TargetCountry)[1:3],
                        multiple = TRUE),
         selectizeInput("metrics1", "Metrics:",
                        choices = c("Click","Conversion","ConversionAC","CR","Rev BR (EURO)","Rev AR (EURO)","ConversionLinear","ConversionACLinear","Rev BR Linear (EURO)","Rev AR Linear (EURO)","ConversionAssisted","ConversionACAssisted","Rev BR Assisted (EURO)","Rev AR Assisted (EURO)"),
                        selected = c("Click"),
                        multiple = TRUE),
         hr(),
         # Filter1 visualization
         h4("Visualization Filters"),
         actionButton("update11", "Update plot"),
         dateRangeInput("dateRange11",
                        label = "Date range:",
                        start = Sys.Date() - 7,
                        end = Sys.Date() - 1,
                        min = "2018-02-24",
                        max = Sys.Date() - 1),
         selectizeInput("metrics11", "Metrics:",
                        choices = c("Click","Conversion","ConversionAC","CR","Rev BR (EURO)","Rev AR (EURO)","ConversionLinear","ConversionACLinear","Rev BR Linear (EURO)","Rev AR Linear (EURO)","ConversionAssisted","ConversionACAssisted","Rev BR Assisted (EURO)","Rev AR Assisted (EURO)"),
                        multiple = FALSE),
         h5("Country-Channel:"),
         selectizeInput('country11', 'Country (the country to check):',
                        choices = unique(data$TargetCountry),
                        multiple = FALSE),
         selectizeInput('channel11', 'Channel (the channel to compare):',
                        choices = c("Affiliate CPC","Affiliate CPO","Direct Access","Display Paid Social","Display Prospecting","Display Retargeting","E-mail","Referrer","SEA Brand","SEA Non-Brand","SEA Shopping","SEO Brand","SEO Non-Brand","Social Media","Social Networks","Transaction Mail"),
                        multiple = TRUE),
         h5("Channel-Country:"),
         selectizeInput('channel12', 'Channel (the channel to check):',
                        choices = c("Affiliate CPC","Affiliate CPO","Direct Access","Display Paid Social","Display Prospecting","Display Retargeting","E-mail","Referrer","SEA Brand","SEA Non-Brand","SEA Shopping","SEO Brand","SEO Non-Brand","Social Media","Social Networks","Transaction Mail"),
                        selected = c("Affiliate CPC"),
                        multiple = FALSE),
         selectizeInput('country12', 'Country (the country to compare):',
                        choices = unique(data$TargetCountry),
                        multiple = TRUE)
       ),
       # Dashboard
       mainPanel(
         width = 10,
         tabsetPanel(
           # Table
           tabPanel("Table",
             tableOutput("dailyreport")
           ),
           # Visualization
           tabPanel("Visualization",
             plotlyOutput("dailyChannel"),
             hr(),
             plotlyOutput("dailyCountryChannel"),
             hr(),
             plotlyOutput("dailyChannelCountry")
           )
         )
       )
     ),
############################################## Monthly report dashboard ##############################################
     tabPanel("Monthly Report",
       # Filter2 table
       sidebarPanel(
         width = 2,
         h4("Table Filters"),
         actionButton("update2", "Update view"),
         sliderInput("month2","Month:",
                     min = 1, max=12,
                     value = c(as.numeric(strftime(Sys.Date()-120, format = "%m")), as.numeric(strftime(Sys.Date(), format = "%m")))),
         selectizeInput("dimensions2","Dimensions:",
                        choices = c("Channel","Source","TargetCountry"),
                        selected = c("Channel"),
                        multiple = TRUE),
         selectizeInput("metrics2", "Metrics:",
                        choices = c("Click","Conversion","ConversionAC","CR","Rev BR (EURO)","Rev AR (EURO)","ConversionLinear","ConversionACLinear","Rev BR Linear (EURO)","Rev AR Linear (EURO)","ConversionAssisted","ConversionACAssisted","Rev BR Assisted (EURO)","Rev AR Assisted (EURO)"),
                        selected = c("Click"),
                        multiple = TRUE),
         selectizeInput('country2', 'Country:', 
                        choices = unique(data$TargetCountry),
                        selected = unique(data$TargetCountry),
                        multiple = TRUE),
         hr(),
         # Filter2 visualization
         h4("Visualization Filters"),
         actionButton("update21", "Update plot"),
         sliderInput("month21","Month:",
                     min = 1, max=12,
                     value = c(as.numeric(strftime(Sys.Date()-120, format = "%m")), as.numeric(strftime(Sys.Date(), format = "%m")))),
         selectizeInput("metrics21", "Metrics:",
                        choices = c("Click","Conversion","ConversionAC","CR","Rev BR (EURO)","Rev AR (EURO)","ConversionLinear","ConversionACLinear","Rev BR Linear (EURO)","Rev AR Linear (EURO)","ConversionAssisted","ConversionACAssisted","Rev BR Assisted (EURO)","Rev AR Assisted (EURO)"),
                        multiple = FALSE),
         h5("Country-Channel:"),
         selectizeInput('country21', 'Country (the country to check):',
                        choices = unique(data$TargetCountry),
                        multiple = FALSE),
         selectizeInput('channel21', 'Channel (the channel to compare):',
                        choices = c("Affiliate CPC","Affiliate CPO","Direct Access","Display Paid Social","Display Prospecting","Display Retargeting","E-mail","Referrer","SEA Brand","SEA Non-Brand","SEA Shopping","SEO Brand","SEO Non-Brand","Social Media","Social Networks","Transaction Mail"),
                        multiple = TRUE),
         h5("Channel-Country:"),
         selectizeInput('channel22', 'Channel (the channel to check):',
                        choices = c("Affiliate CPC","Affiliate CPO","Direct Access","Display Paid Social","Display Prospecting","Display Retargeting","E-mail","Referrer","SEA Brand","SEA Non-Brand","SEA Shopping","SEO Brand","SEO Non-Brand","Social Media","Social Networks","Transaction Mail"),
                        selected = c("Affiliate CPC"),
                        multiple = FALSE),
         selectizeInput('country22', 'Country (the country to compare):',
                        choices = unique(data$TargetCountry),
                        multiple = TRUE)
       ),
       # Dashboard
       mainPanel(
         width = 10,
         tabsetPanel(
           # Table
           tabPanel("Table",
             tableOutput("mediamonthreport")
           ),
           # Visualization
           tabPanel("Visualization",
             plotlyOutput("monthCountryChannel"),
             hr(),
             plotlyOutput("monthChannelCountry")
           )
         )
       )
     ),
############################################## Weekly report dashboard ##############################################
     tabPanel("Weekly Report",
# Filter
       sidebarPanel(
         width = 2,
         h4("Table Filters"),
         actionButton("update3", "Update view"),
         dateRangeInput("dateRange3",
                        label = "Date range:",
                        start = Sys.Date() - 7,
                        end = Sys.Date() - 1,
                        min = "2018-02-24",
                        max = Sys.Date() - 1),
         selectizeInput("dimensions3","Dimensions:",
                        choices = c("Channel","Source","TargetCountry"),
                        selected = c("Channel"),
                        multiple = TRUE),
         selectizeInput("metrics3", "Metrics:",
                        choices = c("Click","Conversion","ConversionAC","CR","Rev BR (EURO)","Rev AR (EURO)","ConversionLinear","ConversionACLinear","Rev BR Linear (EURO)","Rev AR Linear (EURO)","ConversionAssisted","ConversionACAssisted","Rev BR Assisted (EURO)","Rev AR Assisted (EURO)"),
                        selected = c("Click"),
                        multiple = TRUE),
         selectizeInput('country3', 'Country:', 
                        choices = unique(data$TargetCountry),
                        selected = unique(data$TargetCountry),
                        multiple = TRUE),
         hr(),
         # Filter2 visualization
         h4("Visualization Filters"),
         actionButton("update31", "Update plot"),
         dateRangeInput("dateRange31",
                        label = "Date range:",
                        start = Sys.Date() - 7,
                        end = Sys.Date() - 1,
                        min = "2018-02-24",
                        max = Sys.Date() - 1),
         selectizeInput("metrics31", "Metrics:",
                        choices = c("Click","Conversion","ConversionAC","CR","Rev BR (EURO)","Rev AR (EURO)","ConversionLinear","ConversionACLinear","Rev BR Linear (EURO)","Rev AR Linear (EURO)","ConversionAssisted","ConversionACAssisted","Rev BR Assisted (EURO)","Rev AR Assisted (EURO)"),
                        multiple = FALSE),
         h5("Country-Channel:"),
         selectizeInput('country31', 'Country (the country to check):',
                        choices = unique(data$TargetCountry),
                        multiple = FALSE),
         selectizeInput('channel31', 'Channel (the channel to compare):',
                        choices = c("Affiliate CPC","Affiliate CPO","Direct Access","Display Paid Social","Display Prospecting","Display Retargeting","E-mail","Referrer","SEA Brand","SEA Non-Brand","SEA Shopping","SEO Brand","SEO Non-Brand","Social Media","Social Networks","Transaction Mail"),
                        multiple = TRUE),
         h5("Channel-Country:"),
         selectizeInput('channel32', 'Channel (the channel to check):',
                        choices = c("Affiliate CPC","Affiliate CPO","Direct Access","Display Paid Social","Display Prospecting","Display Retargeting","E-mail","Referrer","SEA Brand","SEA Non-Brand","SEA Shopping","SEO Brand","SEO Non-Brand","Social Media","Social Networks","Transaction Mail"),
                        selected = c("Affiliate CPC"),
                        multiple = FALSE),
         selectizeInput('country32', 'Country (the country to compare):',
                        choices = unique(data$TargetCountry),
                        multiple = TRUE)
      ),
# Dashboard
       mainPanel(
         width = 10,
         tabsetPanel(
           # Table
           tabPanel("Table",
                    tableOutput("mediaweekreport")
           ),
           # Visualization
           tabPanel("Visualization",
                    plotlyOutput("weekCountryChannel"),
                    hr(),
                    plotlyOutput("weekChannelCountry")
           )
         )
       )
     ),
############################################## Download data option ##############################################
     tabPanel("Download Data",
# Filter
       sidebarPanel("Filters",
         width = 2,
         dateRangeInput("dateRange4",
           label = "Date range:",
           start = Sys.Date() - 7,
           end = Sys.Date() - 1,
           min = "2018-02-24",
           max = Sys.Date() - 1),
         selectizeInput('country4', 'Country:', 
                        choices = unique(data$TargetCountry),
                        selected = unique(data$TargetCountry),
                        multiple = TRUE),
         downloadButton("downloadData", "Download")
       ),
# Preview data
       mainPanel(
         tabsetPanel(
           tabPanel("Preview Table",
             tableOutput("previewtable")
           )
         )
       )
     )
   )
)

# Server
server <- function(input, output) {
  
  ############################################## daily report table output ##############################################
  dailyReport <- eventReactive(input$update1, {
    agggroup <- c("Day",input$dimensions1)
    data %>%
      filter(TargetCountry %in% input$country1 & Week == paste0("CW",input$week1)) %>%
      group_by_at(vars(one_of(agggroup))) %>%
      summarize(Clicks = sum(Clicks),
                Conversions = sum(Conversions),
                ConversionsAC = sum(ConversionsAC),
                RevGross = sum(RevGross),
                RevNet = sum(RevNet),
                ConversionsLinear = sum(ConversionsLinear),
                ConversionsACLinear = sum(ConversionsACLinear),
                RevGrossLinear = sum(RevGrossLinear),
                RevNetLinear = sum(RevNetLinear),
                ConversionsAssisted = sum(ConversionsAssisted),
                ConversionsACAssisted = sum(ConversionsACAssisted),
                RevGrossAssisted = sum(RevGrossAssisted),
                RevNetAssisted = sum(RevNetAssisted)) %>%
      mutate(Click = format(Clicks, decimal.mark = ".", big.mark = ",", digits = 3),
             Conversion = format(Conversions, decimal.mark = ".", big.mark = ",", digits = 3),
             ConversionAC = format(ConversionsAC, decimal.mark = ".", big.mark = ",", digits = 3),
             CR = percent(ifelse(Clicks == 0, 0, Conversions/Clicks)),
             `Rev BR (EURO)` = format(RevGross, decimal.mark = ".", big.mark = ",", digits = 3),
             `Rev AR (EURO)` = format(RevNet, decimal.mark = ".", big.mark = ",", digits = 3),
             ConversionLinear = format(ConversionsLinear, decimal.mark = ".", big.mark = ",", digits = 3),
             ConversionACLinear = format(ConversionsACLinear, decimal.mark = ".", big.mark = ",", digits = 3),
             `Rev BR Linear (EURO)` = format(RevGrossLinear, decimal.mark = ".", big.mark = ",", digits = 3),
             `Rev AR Linear (EURO)` = format(RevNetLinear, decimal.mark = ".", big.mark = ",", digits = 3),
             ConversionAssisted = format(ConversionsAssisted, decimal.mark = ".", big.mark = ",", digits = 3),
             ConversionACAssisted = format(ConversionsACAssisted, decimal.mark = ".", big.mark = ",", digits = 3),
             `Rev BR Assisted (EURO)` = format(RevGrossAssisted, decimal.mark = ".", big.mark = ",", digits = 3),
             `Rev AR Assisted (EURO)` = format(RevNetAssisted, decimal.mark = ".", big.mark = ",", digits = 3)) %>%
      select(one_of(c("Day",input$dimensions1,input$metrics1))) %>%
      melt(id = c("Day",input$dimensions1)) %>%
      dcast(paste(ifelse(length(input$dimensions1) == 1, input$dimensions1, paste(input$dimensions1[1],"+",input$dimensions1[2])), "~ variable + Day"), value.var = "value")
  })
  
  output$dailyreport <- renderTable({
    dailyReport()
  })
  
  # daily report visualization: country-channel
  dailycountrychannel <- eventReactive(input$update11, {
    data %>%
      filter(TargetCountry == input$country11 & Day >= input$dateRange11[1] & Day <= input$dateRange11[2] & Channel %in% input$channel11) %>%
      group_by(Day,Channel) %>%
      summarize(Click = sum(Clicks),
                Conversion = sum(Conversions),
                ConversionAC = sum(ConversionsAC),
                `Rev BR (EURO)` = sum(RevGross),
                `Rev AR (EURO)` = sum(RevNet),
                ConversionLinear = sum(ConversionsLinear),
                ConversionACLinear = sum(ConversionsACLinear),
                `Rev BR Linear (EURO)` = sum(RevGrossLinear),
                `Rev AR Linear (EURO)` = sum(RevNetLinear),
                ConversionAssisted = sum(ConversionsAssisted),
                ConversionACAssisted = sum(ConversionsACAssisted),
                `Rev BR Assisted (EURO)` = sum(RevGrossAssisted),
                `Rev AR Assisted (EURO)` = sum(RevNetAssisted)) %>%
      select(one_of(c("Day","Channel",input$metrics11))) %>%
      melt(id = c("Day","Channel")) %>%
      ggplot(aes(x = Day, y = value, color = Channel)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values = cbPalette,
                         labels = input$channel11) +
      labs(title = paste(input$country11,": Daily",input$metrics11,"between",input$dateRange11[1],"and",input$dateRange11[2]),
           y = paste0(input$metrics11),
           x = "Time") +
      theme_bw() +
      theme(panel.background = element_blank(),
            legend.title = element_blank(),
            legend.position = "top",
            legend.text=element_text(size=11),
            text = element_text(size=11))
  })
  
  # daily report visualization: channel-country
  dailychannelcountry <- eventReactive(input$update11, {
    data %>%
      filter(TargetCountry %in% input$country12 & Day >= input$dateRange11[1] & Day <= input$dateRange11[2] & Channel == input$channel12) %>%
      group_by(Day,TargetCountry) %>%
      summarize(Click = sum(Clicks),
                Conversion = sum(Conversions),
                ConversionAC = sum(ConversionsAC),
                `Rev BR (EURO)` = sum(RevGross),
                `Rev AR (EURO)` = sum(RevNet),
                ConversionLinear = sum(ConversionsLinear),
                ConversionACLinear = sum(ConversionsACLinear),
                `Rev BR Linear (EURO)` = sum(RevGrossLinear),
                `Rev AR Linear (EURO)` = sum(RevNetLinear),
                ConversionAssisted = sum(ConversionsAssisted),
                ConversionACAssisted = sum(ConversionsACAssisted),
                `Rev BR Assisted (EURO)` = sum(RevGrossAssisted),
                `Rev AR Assisted (EURO)` = sum(RevNetAssisted)) %>%
      select(one_of(c("Day","TargetCountry",input$metrics11))) %>%
      melt(id = c("Day","TargetCountry")) %>%
      ggplot(aes(x = Day, y = value, color = TargetCountry)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values = cbPalette,
                         labels = input$country12) +
      labs(title = paste(input$channel12,": Daily",input$metrics11,"between",input$dateRange11[1],"and",input$dateRange11[2]),
           y = paste0(input$metrics11),
           x = "Time") +
      theme_bw() +
      theme(panel.background = element_blank(),
            legend.title = element_blank(),
            legend.position = "top",
            legend.text=element_text(size=11),
            text = element_text(size=11))
  })
  
  # daily report visualization: channel share
  dailychannel <- eventReactive(input$update11, {
    data %>%
      filter(Day >= input$dateRange11[1] & Day <= input$dateRange11[2] & !Channel %in% c("Display Unknown","SEA Unknown","Other","Undefined")) %>%
      group_by(Day,Channel) %>%
      summarize(Click = sum(Clicks),
                Conversion = sum(Conversions),
                ConversionAC = sum(ConversionsAC),
                `Rev BR (EURO)` = sum(RevGross),
                `Rev AR (EURO)` = sum(RevNet),
                ConversionLinear = sum(ConversionsLinear),
                ConversionACLinear = sum(ConversionsACLinear),
                `Rev BR Linear (EURO)` = sum(RevGrossLinear),
                `Rev AR Linear (EURO)` = sum(RevNetLinear),
                ConversionAssisted = sum(ConversionsAssisted),
                ConversionACAssisted = sum(ConversionsACAssisted),
                `Rev BR Assisted (EURO)` = sum(RevGrossAssisted),
                `Rev AR Assisted (EURO)` = sum(RevNetAssisted)) %>%
      select(one_of(c("Day","Channel",input$metrics11))) %>%
      melt(id = c("Day","Channel")) %>%
      group_by(Day) %>%
      mutate(Share = value/sum(value)) %>%
      ggplot(aes(x = Day, y = Share, fill = Channel)) +
      geom_bar(stat = "identity", width = 1) +
      scale_fill_manual(values = channelPalette) +
      labs(title = paste("Daily",input$metrics11,"by Channel between",input$dateRange11[1],"and",input$dateRange11[2]),
           y = paste0(input$metrics11,"Share"),
           x = "Time") +
      coord_flip() +
      theme_bw() +
      theme(panel.background = element_blank(),
            legend.title = element_blank(),
            legend.text=element_text(size=11),
            text = element_text(size=11))
  })
  
  output$dailyCountryChannel <- renderPlotly({
    # dailyCountryChannel <- 
    dailycountrychannel()
    # dailyCountryChannel$elementId <- NULL
    # dailyCountryChannel
  })
  
  output$dailyChannelCountry <- renderPlotly({
    dailychannelcountry()
  })
  
  output$dailyChannel <- renderPlotly({
    dailychannel()
  })
  
  ############################################## monthly media report table output ##############################################
  mediaMonth <- eventReactive(input$update2, {
    agggroup <- c("Month","monthnumber",input$dimensions2)
    data %>%
      filter(TargetCountry %in% input$country2 & monthnumber >= input$month2[1] & monthnumber <= input$month2[2]) %>%
      group_by_at(vars(one_of(agggroup))) %>%
      summarize(Clicks = sum(Clicks),
                Conversions = sum(Conversions),
                ConversionsAC = sum(ConversionsAC),
                RevGross = sum(RevGross),
                RevNet = sum(RevNet),
                ConversionsLinear = sum(ConversionsLinear),
                ConversionsACLinear = sum(ConversionsACLinear),
                RevGrossLinear = sum(RevGrossLinear),
                RevNetLinear = sum(RevNetLinear),
                ConversionsAssisted = sum(ConversionsAssisted),
                ConversionsACAssisted = sum(ConversionsACAssisted),
                RevGrossAssisted = sum(RevGrossAssisted),
                RevNetAssisted = sum(RevNetAssisted)) %>%
      mutate(Click = format(Clicks, decimal.mark = ".", big.mark = ",", digits = 3),
             Conversion = format(Conversions, decimal.mark = ".", big.mark = ",", digits = 3),
             ConversionAC = format(ConversionsAC, decimal.mark = ".", big.mark = ",", digits = 3),
             CR = percent(ifelse(Clicks == 0, 0, Conversions/Clicks)),
             `Rev BR (EURO)` = format(RevGross, decimal.mark = ".", big.mark = ",", digits = 3),
             `Rev AR (EURO)` = format(RevNet, decimal.mark = ".", big.mark = ",", digits = 3),
             ConversionLinear = format(ConversionsLinear, decimal.mark = ".", big.mark = ",", digits = 3),
             ConversionACLinear = format(ConversionsACLinear, decimal.mark = ".", big.mark = ",", digits = 3),
             `Rev BR Linear (EURO)` = format(RevGrossLinear, decimal.mark = ".", big.mark = ",", digits = 3),
             `Rev AR Linear (EURO)` = format(RevNetLinear, decimal.mark = ".", big.mark = ",", digits = 3),
             ConversionAssisted = format(ConversionsAssisted, decimal.mark = ".", big.mark = ",", digits = 3),
             ConversionACAssisted = format(ConversionsACAssisted, decimal.mark = ".", big.mark = ",", digits = 3),
             `Rev BR Assisted (EURO)` = format(RevGrossAssisted, decimal.mark = ".", big.mark = ",", digits = 3),
             `Rev AR Assisted (EURO)` = format(RevNetAssisted, decimal.mark = ".", big.mark = ",", digits = 3)) %>%
      ungroup() %>%
      select(one_of(c("Month",input$dimensions2,input$metrics2))) %>%
      melt(id = c("Month",input$dimensions2)) %>%
      dcast(paste(ifelse(length(input$dimensions2) == 1, input$dimensions2, ifelse(length(input$dimensions2) == 2, paste(input$dimensions2[1],"+",input$dimensions2[2]), paste(input$dimensions2[1],input$dimensions2[2],input$dimensions2[3],sep = "+"))), "~ variable + Month"), value.var = "value")
  })
  
  output$mediamonthreport <- renderTable({
    mediaMonth()
  })
  
  # monthly report visualization: country-channel
  monthcountrychannel <- eventReactive(input$update21, {
    data %>%
      filter(TargetCountry == input$country21 & monthnumber >= input$month21[1] & monthnumber <= input$month21[2] & Channel %in% input$channel21) %>%
      group_by(Month,Channel) %>%
      summarize(Click = sum(Clicks),
                Conversion = sum(Conversions),
                ConversionAC = sum(ConversionsAC),
                `Rev BR (EURO)` = sum(RevGross),
                `Rev AR (EURO)` = sum(RevNet),
                ConversionLinear = sum(ConversionsLinear),
                ConversionACLinear = sum(ConversionsACLinear),
                `Rev BR Linear (EURO)` = sum(RevGrossLinear),
                `Rev AR Linear (EURO)` = sum(RevNetLinear),
                ConversionAssisted = sum(ConversionsAssisted),
                ConversionACAssisted = sum(ConversionsACAssisted),
                `Rev BR Assisted (EURO)` = sum(RevGrossAssisted),
                `Rev AR Assisted (EURO)` = sum(RevNetAssisted)) %>%
      select(one_of(c("Month","Channel",input$metrics21))) %>%
      melt(id = c("Month","Channel")) %>%
      ggplot(aes(x = Month, y = value, color = Channel)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values = cbPalette,
                         labels = input$channel21) +
      labs(title = paste(input$country21,": Daily",input$metrics21),
           y = paste0(input$metrics21),
           x = "Time") +
      theme_bw() +
      theme(panel.background = element_blank(),
            legend.title = element_blank(),
            legend.position = "top",
            legend.text=element_text(size=11),
            text = element_text(size=11))
  })
  
  # monthly report visualization: channel-country
  monthchannelcountry <- eventReactive(input$update21, {
    data %>%
      filter(TargetCountry %in% input$country22 & monthnumber >= input$month21[1] & monthnumber <= input$month21[2] & Channel == input$channel22) %>%
      group_by(Month,TargetCountry) %>%
      summarize(Click = sum(Clicks),
                Conversion = sum(Conversions),
                ConversionAC = sum(ConversionsAC),
                `Rev BR (EURO)` = sum(RevGross),
                `Rev AR (EURO)` = sum(RevNet),
                ConversionLinear = sum(ConversionsLinear),
                ConversionACLinear = sum(ConversionsACLinear),
                `Rev BR Linear (EURO)` = sum(RevGrossLinear),
                `Rev AR Linear (EURO)` = sum(RevNetLinear),
                ConversionAssisted = sum(ConversionsAssisted),
                ConversionACAssisted = sum(ConversionsACAssisted),
                `Rev BR Assisted (EURO)` = sum(RevGrossAssisted),
                `Rev AR Assisted (EURO)` = sum(RevNetAssisted)) %>%
      select(one_of(c("Month","TargetCountry",input$metrics21))) %>%
      melt(id = c("Month","TargetCountry")) %>%
      ggplot(aes(x = Month, y = value, color = TargetCountry)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values = cbPalette,
                         labels = input$country22) +
      labs(title = paste(input$channel22,": Daily",input$metrics21),
           y = paste0(input$metrics21),
           x = "Time") +
      theme_bw() +
      theme(panel.background = element_blank(),
            legend.title = element_blank(),
            legend.position = "top",
            legend.text=element_text(size=11),
            text = element_text(size=11))
  })
  
  output$monthCountryChannel <- renderPlotly({
    monthcountrychannel()
  })
  
  output$monthChannelCountry <- renderPlotly({
    monthchannelcountry()
  })
  
  ############################################## weekly media report table output ##############################################
  mediaWeek <- eventReactive(input$update3, {
    agggroup <- c("Week",input$dimensions3)
    data %>%
      filter(TargetCountry %in% input$country3 & Day >= input$dateRange3[1] & Day <= input$dateRange3[2]) %>%
      group_by_at(vars(one_of(agggroup))) %>%
      summarize(Clicks = sum(Clicks),
                Conversions = sum(Conversions),
                ConversionsAC = sum(ConversionsAC),
                RevGross = sum(RevGross),
                RevNet = sum(RevNet),
                ConversionsLinear = sum(ConversionsLinear),
                ConversionsACLinear = sum(ConversionsACLinear),
                RevGrossLinear = sum(RevGrossLinear),
                RevNetLinear = sum(RevNetLinear),
                ConversionsAssisted = sum(ConversionsAssisted),
                ConversionsACAssisted = sum(ConversionsACAssisted),
                RevGrossAssisted = sum(RevGrossAssisted),
                RevNetAssisted = sum(RevNetAssisted)) %>%
      mutate(Click = format(Clicks, decimal.mark = ".", big.mark = ",", digits = 3),
             Conversion = format(Conversions, decimal.mark = ".", big.mark = ",", digits = 3),
             ConversionAC = format(ConversionsAC, decimal.mark = ".", big.mark = ",", digits = 3),
             CR = percent(ifelse(Clicks == 0, 0, Conversions/Clicks)),
             `Rev BR (EURO)` = format(RevGross, decimal.mark = ".", big.mark = ",", digits = 3),
             `Rev AR (EURO)` = format(RevNet, decimal.mark = ".", big.mark = ",", digits = 3),
             ConversionLinear = format(ConversionsLinear, decimal.mark = ".", big.mark = ",", digits = 3),
             ConversionACLinear = format(ConversionsACLinear, decimal.mark = ".", big.mark = ",", digits = 3),
             `Rev BR Linear (EURO)` = format(RevGrossLinear, decimal.mark = ".", big.mark = ",", digits = 3),
             `Rev AR Linear (EURO)` = format(RevNetLinear, decimal.mark = ".", big.mark = ",", digits = 3),
             ConversionAssisted = format(ConversionsAssisted, decimal.mark = ".", big.mark = ",", digits = 3),
             ConversionACAssisted = format(ConversionsACAssisted, decimal.mark = ".", big.mark = ",", digits = 3),
             `Rev BR Assisted (EURO)` = format(RevGrossAssisted, decimal.mark = ".", big.mark = ",", digits = 3),
             `Rev AR Assisted (EURO)` = format(RevNetAssisted, decimal.mark = ".", big.mark = ",", digits = 3)) %>%
      select(one_of(c("Week",input$dimensions3,input$metrics3))) %>%
      melt(id = c("Week",input$dimensions3)) %>%
      dcast(paste(ifelse(length(input$dimensions3) == 1, input$dimensions3, ifelse(length(input$dimensions3) == 2, paste(input$dimensions3[1],"+",input$dimensions3[2]), paste(input$dimensions3[1],input$dimensions3[2],input$dimensions3[3],sep = "+"))), "~ variable + Week"), value.var = "value")
  })
  
  output$mediaweekreport <- renderTable({
    mediaWeek()
  })
  
  # weekly report visualization: country-channel
  weekcountrychannel <- eventReactive(input$update31, {
    data %>%
      filter(TargetCountry == input$country31 & Day >= input$dateRange31[1] & Day <= input$dateRange31[2] & Channel %in% input$channel31) %>%
      group_by(Week,Channel) %>%
      summarize(Click = sum(Clicks),
                Conversion = sum(Conversions),
                ConversionAC = sum(ConversionsAC),
                `Rev BR (EURO)` = sum(RevGross),
                `Rev AR (EURO)` = sum(RevNet),
                ConversionLinear = sum(ConversionsLinear),
                ConversionACLinear = sum(ConversionsACLinear),
                `Rev BR Linear (EURO)` = sum(RevGrossLinear),
                `Rev AR Linear (EURO)` = sum(RevNetLinear),
                ConversionAssisted = sum(ConversionsAssisted),
                ConversionACAssisted = sum(ConversionsACAssisted),
                `Rev BR Assisted (EURO)` = sum(RevGrossAssisted),
                `Rev AR Assisted (EURO)` = sum(RevNetAssisted)) %>%
      select(one_of(c("Week","Channel",input$metrics31))) %>%
      melt(id = c("Week","Channel")) %>%
      ggplot(aes(x = Week, y = value, color = Channel)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values = cbPalette,
                         labels = input$channel31) +
      labs(title = paste(input$country31,": Weekly",input$metrics31),
           y = paste0(input$metrics31),
           x = "Time") +
      theme_bw() +
      theme(panel.background = element_blank(),
            legend.title = element_blank(),
            legend.position = "top",
            legend.text=element_text(size=11),
            text = element_text(size=11))
  })
  
  # weekly report visualization: channel-country
  weekchannelcountry <- eventReactive(input$update31, {
    data %>%
      filter(TargetCountry %in% input$country32 & Day >= input$dateRange31[1] & Day <= input$dateRange31[2] & Channel == input$channel32) %>%
      group_by(Week,TargetCountry) %>%
      summarize(Click = sum(Clicks),
                Conversion = sum(Conversions),
                ConversionAC = sum(ConversionsAC),
                `Rev BR (EURO)` = sum(RevGross),
                `Rev AR (EURO)` = sum(RevNet),
                ConversionLinear = sum(ConversionsLinear),
                ConversionACLinear = sum(ConversionsACLinear),
                `Rev BR Linear (EURO)` = sum(RevGrossLinear),
                `Rev AR Linear (EURO)` = sum(RevNetLinear),
                ConversionAssisted = sum(ConversionsAssisted),
                ConversionACAssisted = sum(ConversionsACAssisted),
                `Rev BR Assisted (EURO)` = sum(RevGrossAssisted),
                `Rev AR Assisted (EURO)` = sum(RevNetAssisted)) %>%
      select(one_of(c("Week","TargetCountry",input$metrics31))) %>%
      melt(id = c("Week","TargetCountry")) %>%
      ggplot(aes(x = Week, y = value, color = TargetCountry)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values = cbPalette,
                         labels = input$country32) +
      labs(title = paste(input$channel32,": Weekly",input$metrics31),
           y = paste0(input$metrics31),
           x = "Time") +
      theme_bw() +
      theme(panel.background = element_blank(),
            legend.title = element_blank(),
            legend.position = "top",
            legend.text=element_text(size=11),
            text = element_text(size=11))
  })
  
  output$weekCountryChannel <- renderPlotly({
    weekcountrychannel()
  })
  
  output$weekChannelCountry <- renderPlotly({
    weekchannelcountry()
  })  
  
  ############################################## data download ##############################################
  # filter data to be downloaded
  downloaddata <- reactive({
    filter(data, TargetCountry %in% input$country4 & Day >= input$dateRange4[1] & Day <= input$dateRange4[2])
  })
  
  # create data table to be download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("PerformanceMarketing",paste(as.character(input$dateRange4), collapse = "to"),".csv")
    },
    content = function(file) {
      write.csv2(downloaddata(), file, row.names = FALSE)
    }
  )
  
  # preview data to be downloaded
  output$previewtable <- renderTable({
    data %>%
      filter(TargetCountry %in% input$country4 & Day >= input$dateRange4[1] & Day <= input$dateRange4[2]) %>%
      head(n = 25L)
  })
}

# Define APP
app <- shinyApp(ui = ui, server = server)

# Run APP
runApp(app)
