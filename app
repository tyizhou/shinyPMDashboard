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

# Database connection
con <- odbcDriverConnect("driver=SQL Server;server=xxxx;Uid=xxxx;Pwd=xxxx")

# Queries
query <- "
select 
  Crunchdate as Day,
  Week,
  Month,
  Channel,
  Source,
  TargetCountry,
  OrderStatus,
  sum(Clicks) as Clicks,
  sum(Conversions) as Conversions,
  sum(RevGross) as RevGross,
  sum(RevNet) as RevNet
from
  CDM.Report.CA_PM_Report
group by Crunchdate,Week,Month,Channel,Source,TargetCountry,OrderStatus"

# Run queries to get data from database
data <- sqlQuery(con, query)

# ui
ui <- tagList(
   navbarPage("Report",
# Daily report dashboard
     tabPanel("Daily Report",
# Filter
       sidebarPanel("Filters",
         width = 2,
         sliderInput("week","Week:",
                     min = 1, max=52,
                     value = as.numeric(strftime(Sys.Date()-7, format = "%V"))),
         selectizeInput("dimensions","Dimensions:",
                        choices = c("TargetCountry","Channel"),
                        selected = c("TargetCountry","Channel"),
                        multiple = TRUE),
         selectizeInput('country', 'Country:', 
                        choices = unique(data$TargetCountry),
                        selected = unique(data$TargetCountry)[1:3],
                        multiple = TRUE),
         checkboxGroupInput("metrics", "Metrics:",
                            c("Click","Conversion","CR","Rev BR (EURO)","Rev AR (EURO)"), selected = c("Click")),
         selectizeInput('channel', 'Channel (visualization only):',
                        choices = c("Affiliate CPC","Affiliate CPO","Direct Access","Display Paid Social","Display Prospecting","Display Retargeting","E-mail","Referrer","SEA Brand","SEA Non-Brand","SEA Shopping","SEO Brand","SEO Non-Brand","Social Media","Social Networks","Transaction Mail"),
                        selected = c("Affiliate CPC"),
                        multiple = TRUE)
       ),
# Dashboard
       mainPanel(
         tabsetPanel(
           tabPanel("Table",
             tableOutput("dailyreport")
           ),
           tabPanel("Visualization",
             plotOutput("dailyplot")        
           )
         )
       )
     ),
# Media monthly report dashboard
     tabPanel("Media Month",
# Filter
       sidebarPanel("Filters",
         width = 2,
         sliderInput("month","Month:",
                     min = 1, max=12,
                     value = c(as.numeric(strftime(Sys.Date()-120, format = "%m")), as.numeric(strftime(Sys.Date(), format = "%m")))),
         selectizeInput("dimensions","Dimensions:",
                        choices = c("Channel","Source","TargetCountry"),
                        selected = c("Channel","Source","TargetCountry"),
                        multiple = TRUE),
         checkboxGroupInput("metrics", "Metrics:",
                            c("Click","Conversion","CR","Rev BR (EURO)","Rev AR (EURO)"), selected = c("Click")),
         selectizeInput('country', 'Country:', 
                        choices = unique(data$TargetCountry),
                        selected = unique(data$TargetCountry),
                        multiple = TRUE),
         selectizeInput('channel', 'Channel (visualization only):',
                        choices = c("Affiliate CPC","Affiliate CPO","Direct Access","Display Paid Social","Display Prospecting","Display Retargeting","E-mail","Referrer","SEA Brand","SEA Non-Brand","SEA Shopping","SEO Brand","SEO Non-Brand","Social Media","Social Networks","Transaction Mail"),
                        selected = c("Affiliate CPC"),
                        multiple = TRUE)
       ),
# Dashboard
       mainPanel(
         tabsetPanel(
           tabPanel("Table",
             tableOutput("mediamonthreport")
           ),
           tabPanel("Visualization",
             plotOutput("mediamonthplot")
           )
         )
       )
     ),
# Media weekly report dashboard
     tabPanel("Media Week",
# Filter
       sidebarPanel("Filters",
         width = 2,
         sliderInput("week","Week:",
                     min = 1, max=52,
                     value = c(as.numeric(strftime(Sys.Date()-28, format = "%V")),as.numeric(strftime(Sys.Date()-7, format = "%V")))),
         selectizeInput('country', 'Country:', 
                        choices = c("Germany","United Kingdom","United States"),
                        multiple = TRUE),
         checkboxGroupInput("metrics", "Metrics:",
                            c("Click","Conversion","CR","Rev BR (EURO)","Rev AR (EURO)"), selected = c("Click")),
         selectizeInput('channel', 'Channel (visualization only):',
                        choices = c("Affiliate CPC","Affiliate CPO","Direct Access","Display Paid Social","Display Prospecting","Display Retargeting","E-mail","Referrer","SEA Brand","SEA Non-Brand","SEA Shopping","SEO Brand","SEO Non-Brand","Social Media","Social Networks","Transaction Mail"),
                        selected = c("Affiliate CPC"),
                        multiple = TRUE)
      ),
# Dashboard
       mainPanel(
         tabsetPanel(
           tabPanel("Table"
           ),
           tabPanel("Visualization"
           )
         )
       )
     ),
# Country monthly report dashboard
     tabPanel("Country Month",
# Filter
       sidebarPanel("Filters",
         width = 2,
         sliderInput("month","Month:",
                     min = 1, max=52,
                     value = c(as.numeric(strftime(Sys.Date()-120, format = "%m")), as.numeric(strftime(Sys.Date(), format = "%m")))),
         selectizeInput('country', 'Country:', 
                        choices = c("Germany","United Kingdom","United States"),
                        multiple = TRUE),
         checkboxGroupInput("metrics", "Metrics:",
                            c("Click","Conversion","CR","Rev BR (EURO)","Rev AR (EURO)"), selected = c("Click")),
         selectizeInput('channel', 'Channel (visualization only):',
                        choices = c("Affiliate CPC","Affiliate CPO","Direct Access","Display Paid Social","Display Prospecting","Display Retargeting","E-mail","Referrer","SEA Brand","SEA Non-Brand","SEA Shopping","SEO Brand","SEO Non-Brand","Social Media","Social Networks","Transaction Mail"),
                        selected = c("Affiliate CPC"),
                        multiple = TRUE)
       ),
# Dashboard
       mainPanel(
         tabsetPanel(
           tabPanel("Table"
           ),
           tabPanel("Visualization"
           )
         )
       )
     ),
# Country weekly report dashboard
     tabPanel("Country Week",
# Filter
       sidebarPanel("Filters",
         width = 2,
         sliderInput("week","Week:",
                     min = 1, max=52,
                     value = c(as.numeric(strftime(Sys.Date()-28, format = "%V")),as.numeric(strftime(Sys.Date()-7, format = "%V")))),
         selectizeInput('country', 'Country:', 
                        choices = c("Germany","United Kingdom","United States"),
                        multiple = TRUE),
         checkboxGroupInput("metrics", "Metrics:",
                            c("Click","Conversion","CR","Rev BR (EURO)","Rev AR (EURO)"), selected = c("Click")),
         selectizeInput('channel', 'Channel (visualization only):',
                        choices = c("Affiliate CPC","Affiliate CPO","Direct Access","Display Paid Social","Display Prospecting","Display Retargeting","E-mail","Referrer","SEA Brand","SEA Non-Brand","SEA Shopping","SEO Brand","SEO Non-Brand","Social Media","Social Networks","Transaction Mail"),
                        selected = c("Affiliate CPC"),
                        multiple = TRUE)
       ),
# Dashboard
       mainPanel(
         tabsetPanel(
           tabPanel("Table"
           ),
           tabPanel("Visualization"
           )
         )
       )
     ),
# Download data option
     tabPanel("Downloading Data",
# Filter
       sidebarPanel("Filters",
         width = 2,
         dateRangeInput("dateRange",
           label = "Date range:",
           start = Sys.Date() - 7,
           end = Sys.Date() - 1,
           min = "2018-02-24",
           max = Sys.Date() - 1),
         selectizeInput('country', 'Country:', 
                        choices = c("Germany","United Kingdom","United States"),
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
  
  output$dailyreport <- renderTable({
 
    agggroup <- c("Day","Week","Month",input$dimensions)
       
    data %>%
      filter(TargetCountry %in% input$country) %>%
      group_by_at(vars(one_of(agggroup))) %>%
      summarize(Clicks = sum(Clicks),
                Conversions = sum(Conversions),
                RevGross = sum(RevGross),
                RevNet = sum(RevNet)) %>%
      mutate(Click = format(Clicks, decimal.mark = ".", big.mark = ","),
             Conversion = format(Conversions, decimal.mark = ".", big.mark = ","),
             CR = percent(Conversions/Clicks),
             `Rev BR (EURO)` = format(RevGross, decimal.mark = ".", big.mark = ","),
             `Rev AR (EURO)` = format(RevNet, decimal.mark = ".", big.mark = ",")) %>%
      filter(Week == paste0("CW",input$week)) %>%
      select(one_of(c("Day",input$dimensions,"Click","Conversion","CR","Rev BR (EURO)","Rev AR (EURO)"))) %>%
      melt(id = c("Day",input$dimensions)) %>%
      filter(variable %in% input$metrics) %>%
      dcast(paste(ifelse(length(input$dimensions) == 1, input$dimensions, paste(input$dimensions[1],"+",input$dimensions[2])), "~ variable + Day"), value.var = "value")
  })
  
  output$dailyplot <- renderPlot({
    data %>%
      filter(TargetCountry %in% input$country) %>%
      group_by_at(vars(one_of(agggroup))) %>%
      summarize(Clicks = sum(Clicks),
                Conversions = sum(Conversions),
                RevGross = sum(RevGross),
                RevNet = sum(RevNet)) %>%
      mutate(Click = format(Clicks, decimal.mark = ".", big.mark = ","),
             Conversion = format(Conversions, decimal.mark = ".", big.mark = ","),
             CR = percent(Conversions/Clicks),
             `Rev BR (EURO)` = format(RevGross, decimal.mark = ".", big.mark = ","),
             `Rev AR (EURO)` = format(RevNet, decimal.mark = ".", big.mark = ",")) %>%
      filter(Week == paste0("CW",input$week) & Channel %in% input$channel) %>%
      select(Day,Channel,Click,Conversion,CR,`Rev BR (EURO)`,`Rev AR (EURO)`) %>% 
      melt(id = c("Day","Channel")) %>%
      filter(variable %in% input$metrics) %>%
      ggplot(aes(x = Day, y = value, group = Channel, label = value)) +
      geom_line() +
      geom_text(hjust = 0) +
      labs(title = paste("Daily",input$metrics,"in","CW",input$week),
           y = paste0(input$metrics)) +
      theme_bw()
  })
  
  downloaddata <- reactive({
    filter(data, TargetCountry %in% input$country & Day >= input$dateRange[1] & Day <= input$dateRange[2])
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("PerformanceMarketing",paste(as.character(input$dateRange), collapse = "to"),".csv")
    },
    content = function(file) {
      write.csv2(downloaddata(), file, row.names = FALSE)
    }
  )
  
  output$previewtable <- renderTable({
    data %>%
      filter(TargetCountry %in% input$country & Day >= input$dateRange[1] & Day <= input$dateRange[2]) %>%
      head(n = 25L)
  })
  
}

# Define APP
app <- shinyApp(ui = ui, server = server)

# Run APP
runApp(app)
