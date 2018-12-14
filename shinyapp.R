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

# input.csv <- tk_choose.files(default = '',
#                                caption = "Please select the input excel file")
# 
# out.dir <- tk_choose.dir(default = getwd(),
#                          caption = "Where should the output be saved?")

input.excel <- "~/Documents/Trend_Tracking_Lab_Tests/2016-2017-2018_LMh-Activity_Barnaby.csv"
out.dir <- "~/Documents/Trend_Tracking_Lab_Tests/"

# Test that there is at least one argument, if not throw an error.

if (length(input.excel) == 0) {
    stop("You need to add an input Excel file and a output location",
        call. = FALSE)
}


CreateDF <- function(data) {
  
  # Create df from data
  
  data.cleaned <- data[c("SIDC09.Hospital", "SIDC13.Investigation.Requested", "SIDC16.Date.Sample.Received", "Reported.date", "Real.Billing.category")]
  
  # Give better header names
  
  colnames(data.cleaned) <- c("Hospital", "Investigation", "Sample.Received", "Test.Report.Produced", "Billing.Category")
  data.cleaned$Sample.Received <- as.Date(data.cleaned$Sample.Received, format = "%d/%m/%Y")
  data.cleaned$Test.Report.Produced <- as.Date(data.cleaned$Test.Report.Produced, format = "%d/%m/%Y")

  # Remove tests done in 1899 (erroneous)
  
  data.cleaned$Year.Reported <- format(as.Date(data.cleaned[["Test.Report.Produced"]]), "%Y")
  data.cleaned$Month.Reported <-format(as.Date(data.cleaned[["Test.Report.Produced"]]), "%m")
  data.cleaned$Day.Reported <- format(as.Date(data.cleaned[["Test.Report.Produced"]]), "%d")
  data.cleaned <- data.cleaned[!(data.cleaned$Year.Reported == "1899"),]
  data.cleaned <- data.cleaned[!(data.cleaned$Investigation == "#N/A"),]
  data.cleaned <- data.cleaned[!(data.cleaned$Hospital == "Not stated"),]
  data.cleaned <- data.cleaned[!(data.cleaned$Hospital == "5"),]
  
  # Calculate turnaround time in days
  
  data.cleaned$Turnover.Time <- as.Date(data.cleaned[["Test.Report.Produced"]]) - as.Date(data.cleaned[["Sample.Received"]])
  data.cleaned$Turnover.Time <- as.numeric(data.cleaned$Turnover.Time)
  
  # Create a "Year.Month" column to label the x-axis with
  
  data.cleaned$Year.Month <- paste("Year ", data.cleaned$Year.Reported, ", Month ", data.cleaned$Month.Reported, sep="")
  data.cleaned$Target.Turnaround <- (30)
  data.cleaned <- na.omit(data.cleaned)
  
  return(data.cleaned)
  
}

data <- read.csv(input.excel)  # Get data frame from input excel file
data.cleaned <- CreateDF(data)


TurnoverPlot <- function(i) {
  ggplot(data.cleaned[data.cleaned$Investigation == i,],
    aes(x = Year.Month, y = Turnover.Time)) +
  labs(title = i, subtitle = "Outliers, defined as ±1.5*IQR, output have not been plotted. Boxplots \nrepresent Median, Q1-Q3 and 1.5*Q1Q3. Red line is the mean.",
    y = "Turnaround Time (Days)",
    x = "Year and Month") +
  geom_boxplot(alpha = 0.80, outlier.colour = "black",
    outlier.shape = NA, outlier.size = .5, notch = FALSE,
    fill = "lightskyblue2", color = "black") +
  stat_summary(aes(y = Turnover.Time, group = 1), fun.y = mean,
    colour="red", geom="line", group = 1) +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(), 
      panel.grid.major.y = element_line(size = .1, color = "black")) +
    scale_x_discrete(limits = sort(unique(data$Year.Month)),
    breaks = sort(unique(data$Year.Month))) +
    ylim(-5, quantile(data.cleaned[data.cleaned$Turnover.Time == i,][[3]])[[4]]+10) +
    geom_hline(yintercept = data$Target.Turnaround, linetype = "dashed",
    color = "red", size = .5)
}

# table(data.cleaned$Investigation)

TestsPerYear <- function(i) {
  Investigation <- data[c("Investigation", "Year_Reported", "Month_Reported")]
  Investigation <- as.data.frame(table(Investigation))
  graph <- ggplot(Investigation[Investigation$Investigation == i,], 
             aes(Month.Reported, Freq, group = Year.Reported, color = Year.Reported)) +
           labs(title = i, y = "Frequency", x = "Month") + geom_point() +
           geom_line() + theme_minimal() +
           theme(axis.text.x = element_text(),
             panel.grid.major.x = element_blank(),
             panel.grid.major.y = element_line(size = .1, color = "black"))
  print(graph)
}


# Define UI for slider app


ui <- fluidPage(
  titlePanel("trendtracker"),
  sidebarLayout(
    sidebarPanel(
      
      # Allows you to select a test type
      
      selectInput("turnover.time", "Test Name:",
                  sort(unique(data.cleaned$Investigation))),
      
      # Allows you to select a range of dates
      
      sliderInput("range", "Month:",
                  min = 0, max = 36,
                  value = c(12, 24))
      ),
    
    # Main panel for plotting the graphs
    
    mainPanel(
      tableOutput("values"),
      plotOutput(outputId = "turnoverPlot"),
      plotOutput(outputId = "testsPerYear")
    )
  )
)

# Define server logic for slider examples

server <- function(input, output) {
  
  # Produces a table for the interactive values
  
  sliderValues <- reactive({
    data.frame(
      Unit = c("Month"),
      Range = as.character(c(paste(input$range, collapse = " to "))),
      stringsAsFactors = FALSE)
  })
    
  # Pass slider values to the display
  
  output$values <- renderTable({
    sliderValues()
  })
  
  # Information for the plots
  
  output$turnoverPlot <- renderPlot({(TurnoverPlot(input$turnover.time))})
  output$testsPerYear <- renderPlot({(TestsPerYear(input$turnover.time))})
  
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
