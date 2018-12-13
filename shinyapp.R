#!/usr/bin/env Rscript

# TODO(Callum):
#   Finish the shiny app

##### Install packages, load libraries, get file paths #####

# Function to install packages (if not installed already) and load them


getPackages <- function(required.packages) {
  packages.not.installed <- 
    required.packages[!(required.packages %in% installed.packages()[, "Package"])]
  if(length(packages.not.installed)){
    install.packages(packages.not.installed)}
  lapply(required.packages, require, character.only = TRUE)
}


getPackages(c("tcltk", "reshape2", "lubridate", "xts", "data.table",
              "ggplot2", "gridExtra", "openxlsx", "shiny"))


# input.excel <- tk_choose.files(default = '',
#                                caption = "Please select the input excel file")
# 
# out.dir <- tk_choose.dir(default = getwd(),
#                          caption = "Where should the output be saved?")

input.excel <- "~/Documents/Trend_Tracking_Lab_Tests/2016-2017-2018_LMh-Activity_Barnaby.xlsx"
out.dir <- "~/Documents/Trend_Tracking_Lab_Tests/"

# Test that there is at least one argument, if not throw an error.

if (length(input.excel) == 0) {
  stop("You need to add an input Excel file and a output location",
       call. = FALSE)
}

data <- read.xlsx(input.excel)  # Get data frame from input excel file


createDF <- function(data) {
  Audit <- data[c("SIDC09.Hospital", "SIDC13.Investigation.Requested", "SIDC16.Date.Sample.Received", "Reported.date", "Real.Billing.category")]
  colnames(Audit) <- c("Hospital", "Investigation", "Sample_Received", "Test_Reported", "Billing_Category")
  Audit$Test_Reported <- as.Date(Audit$Test_Reported)
  Audit$Sample_Received <- as.Date(Audit$Sample_Received)
  Audit$Turnover_Time <- Audit$Test_Reported - Audit$Sample_Received
  Audit$Turnover_Time <- as.numeric(Audit$Turnover_Time)
  Audit$Year_Reported <- format(as.Date(Audit$Test_Reported, format="%d/%m/%Y"),"%Y")
  Audit$Month_Reported <- format(as.Date(Audit$Test_Reported, format="%d/%m/%Y"),"%m")
  Audit$Day_Reported <- format(as.Date(Audit$Test_Reported, format="%d/%m/%Y"),"%d")
  Audit[9] <- paste("Y", Audit$Year_Reported, "_M", Audit$Month_Reported, sep="")
  colnames(Audit)[9] <- "Year_Month"
  Audit[10] <- (30)
  colnames(Audit)[10] <- "Target_Turnaround"
  Audit <- na.omit(Audit)
  return(Audit)
}


data <- createDF(data)
turnoverTime <- data[c("Investigation", "Year_Month", "Turnover_Time", "Target_Turnaround")]
turnoverTime <- na.omit(turnoverTime)
graph <- ggplot(turnoverTime[turnoverTime$Investigation == "Translocation Assay (QPCR-CML)",],
    aes(x = Year_Month, y = Turnover_Time)) +
    labs(title = "Translocation Assay (QPCR-CML)", subtitle = "Outliers, defined as ±1.5*IQR, output have not been plotted. Boxplots \nrepresent Median, Q1-Q3 and 1.5*Q1Q3. Red line is the mean.",
        y = "Turnaround Time (Days)",
        x = "Year and Month") +
    geom_boxplot(alpha = 0.80, outlier.colour = "black",
        outlier.shape = NA, outlier.size = .5, notch = FALSE,
    fill = "lightskyblue2", color = "black") +
    stat_summary(aes(y = Turnover_Time, group = 1), fun.y = mean,
        colour="red", geom="line", group = 1) +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = .1, color = "black")) +
    scale_x_discrete(limits = sort(unique(data$Year_Month)),
        breaks = sort(unique(data$Year_Month))) +
    ylim(-5, quantile(turnoverTime[turnoverTime$Investigation == "Translocation Assay (QPCR-CML)",][[3]])[[4]]+10) +
    geom_hline(yintercept = data$Target_Turnaround,
        linetype = "dashed", color = "red", size = .5)


# Define UI for slider demo app

ui <- fluidPage(
  
  # App title ----
  titlePanel("trendtracker"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      sliderInput("integer", "Integer:",
                  min = 0, max = 1000,
                  value = 500,
                  animate = animationOptions(interval = 10, loop = TRUE))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      #tableOutput("values")
      #imageOutput("~/Documents/Trend_Tracking_Lab_Tests/ouput/Billing Categories.pdf")
      plotOutput(outputId = "distPlot")
    )
  )
)


# Define server logic for slider examples ----


server <- function(input, output) {
# Reactive expression to create data frame of all input values ----
    sliderValues <- reactive({
        data.frame(Name = c("Integer", "Decimal", "Range", 
                            "Custom Format", "Animation"),
                   Value = as.character(c(input$integer,
                                   input$decimal,
                                   paste(input$range, collapse = " "),
                                   input$format,
                                   input$animation)),
            stringsAsFactors = FALSE)})

# Show the values in an HTML table ----

output$values <- renderTable({sliderValues()})
output$distPlot <- renderPlot({plot(graph)})

}

shinyApp(ui = ui, server = server)


# runExample("01_hello")      # a histogram
# runExample("02_text")       # tables and data frames
# runExample("03_reactivity") # a reactive expression
# runExample("04_mpg")        # global variables
# runExample("05_sliders")    # slider bars
# runExample("06_tabsets")    # tabbed panels
# runExample("07_widgets")    # help text and submit buttons
# runExample("08_html")       # Shiny app built from HTML
# runExample("09_upload")     # file upload wizard
# runExample("10_download")   # file download wizard
# runExample("11_timer")      # an automated timer
