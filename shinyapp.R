#!/usr/bin/env Rscript

# TODO(Callum):
#   Add a download button that produces a file based on user input

##### Install packages, load libraries, get file paths #####

# Function to install packages (if not installed already) and load them


GetPackages <- function(required.packages) {
  packages.not.installed <- 
    required.packages[!(required.packages %in% installed.packages()[, "Package"])]
  if(length(packages.not.installed)){
    install.packages(packages.not.installed)}
  lapply(required.packages, require, character.only = TRUE)
}


GetPackages(c("tcltk", "reshape2", "lubridate", "xts", "data.table",
              "ggplot2", "gridExtra", "openxlsx", "shiny", "shinyWidgets"))

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
  
  data.cleaned <- data[c("SIDC09.Hospital", "SIDC13.Investigation.Requested",
    "SIDC16.Date.Sample.Received", "Reported.date", "Real.Billing.category")]
  
  # Give better header names
  
  colnames(data.cleaned) <- c("Hospital", "Investigation", "Sample.Received",
    "Test.Report.Produced", "Billing.Category")
  data.cleaned$Sample.Received <- as.Date(data.cleaned$Sample.Received,
    format = "%d/%m/%Y")
  data.cleaned$Test.Report.Produced <- as.Date(data.cleaned$Test.Report.Produced,
    format = "%d/%m/%Y")

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
  
  data.cleaned$Year.Month <- paste("Year ", data.cleaned$Year.Reported, ", Month ", 
    data.cleaned$Month.Reported, sep="")
  data.cleaned$Target.Turnaround <- (10)
  data.cleaned <- na.omit(data.cleaned)
  return(data.cleaned)
  
}


data <- read.csv(input.excel)  # Get data frame from input excel file
data.cleaned <- CreateDF(data)


TurnoverPlot <- function(test, date) {
  plot.data <- data.cleaned[data.cleaned$Investigation == test,]
  plot.data <- plot.data[plot.data$Test.Report.Produced>=date[1] 
    & plot.data$Test.Report.Produced<=date[2],]
  dates <- data.cleaned[data.cleaned$Test.Report.Produced>=date[1]
    & data.cleaned$Test.Report.Produced<=date[2],]
  ggplot(plot.data,
    aes(x = Year.Month, y = Turnover.Time)) +
  labs(title = test, subtitle = "Outliers, defined as ±1.5*IQR, output have not been plotted. Boxplots \nrepresent Median, Q1-Q3 and 1.5*Q1Q3. Red line is the mean.",
    y = "Turnaround Time (Days)",
    x = "Year and Month") +
  geom_boxplot(alpha = 0.80, outlier.colour = "black",
    outlier.shape = NA, outlier.size = .5, notch = FALSE,
    fill = "lightskyblue2", color = "black") +
  stat_summary(aes(y = Turnover.Time, group = 1), fun.y = mean,
    colour="red", geom="line", linetype = "dashed", group = 1) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(size = .1, color = "black")) +
  scale_x_discrete(limits = sort(unique(dates$Year.Month)),
    breaks = sort(unique(dates$Year.Month))) +
  ylim(-5, (quantile(plot.data$Turnover.Time)[[4]] + 10)) +
  geom_hline(yintercept = plot.data$Target.Turnaround[1], linetype = "solid",
    color = "red", size = .5)
}


TestsPerYear <- function(test, date) {
  Investigation <- data.cleaned[c("Investigation", "Test.Report.Produced",
    "Year.Reported", "Month.Reported")]
  Investigation <- Investigation[Investigation$Investigation == test,]
  Investigation <- Investigation[Investigation$Test.Report.Produced>=date[1]
    & Investigation$Test.Report.Produced<=date[2],]
  Investigation <- Investigation[c("Year.Reported", "Month.Reported")]
  Investigation <- as.data.frame(table(Investigation))
  Investigation$Year.Month <- paste(Investigation$Year.Reported, ".", 
    Investigation$Month.Reported, sep = "")
  graph <- ggplot(Investigation[which(Investigation$Freq>0),],  # remove freq 0 values
    aes(x = Year.Month, y = Freq, group = Year.Reported, color = Year.Reported)) +
    labs(title = test, y = "Frequency", x = "Year.Month") + geom_point() +
    geom_line() + theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size = .1, color = "black"))
  print(graph)
}


##### Plot number of billing types per month, year on year #####


BillingType <- function(bill, date) {
  workFreq <- data.cleaned[c("Billing.Category", "Test.Report.Produced", "Year.Reported", "Month.Reported")]
  workFreq <- workFreq[workFreq$Test.Report.Produced >= date[1] & workFreq$Test.Report.Produced <= date[2],]
  workFreq <- workFreq[c("Billing.Category", "Year.Reported", "Month.Reported")]
  workFreq <- as.data.frame(table(workFreq))
  workFreq <- workFreq[workFreq$Billing.Category == bill,]
  workFreq$Year.Month <- paste(workFreq$Year.Reported, ".", workFreq$Month.Reported, sep = "")
  graph <- ggplot(workFreq[which(workFreq$Freq>0),],
    aes(x = Year.Month, y = Freq, group = Year.Reported, color = Year.Reported)) +
    labs(title = bill, y = "Frequency (All Tests)", x = "Year.Month") +
    geom_point() + geom_line() + theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size = .1, color = "black"))
  print(graph)
}


##### Plot the trends in the tests ordered by the top 10 referrer hospitals #####


TopReferrers <- function(referrer, date) {
  topHospitals <<- data.cleaned[c("Hospital")]
  topHospitals <<- as.data.frame(table(topHospitals))
  topHospitals <<- topHospitals[ave(-topHospitals$Freq, FUN = rank) <= 10,]
  referrerHospital <- data.cleaned[c(
    "Hospital", "Test.Report.Produced", "Year.Reported", "Month.Reported")]
  referrerHospital <- referrerHospital[
    referrerHospital$Test.Report.Produced >= date[1] & referrerHospital$Test.Report.Produced <= date[2]
    ,]
  referrerHospital <- referrerHospital[c(
    "Hospital", "Year.Reported", "Month.Reported")]
  referrerHospital <- as.data.frame(table(referrerHospital))
  referrerHospital <- subset(referrerHospital, Hospital %in% topHospitals$topHospitals)
  referrerHospital <- referrerHospital[referrerHospital$Hospital == referrer,]
  referrerHospital$Year.Month <- paste(
    referrerHospital$Year.Reported, ".", referrerHospital$Month.Reported, sep = "")
  graph <- ggplot(referrerHospital[which(referrerHospital$Freq>0),],
    aes(x = Year.Month, y = Freq, group = Year.Reported, color = Year.Reported)) +
    labs(title = referrer, y = "Frequency (All Tests)", x = "Year.Month") +
    geom_point() + geom_line() + theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size = .1, color = "black"))
  print(graph)
}

CreateDownloadTable <- function(data, date, referrer, bill){
  createDownloadTable <- data[
    data$Test.Report.Produced >= date[1] & data$Test.Report.Produced <= date[2]
    ,]
  createDownloadTable <-createDownloadTable[
    createDownloadTable$Hospital == referrer
    ,]
  createDownloadTable <- createDownloadTable[
    createDownloadTable$Billing.Category == bill
    ,]
  print(createDownloadTable)
}

CreateDownloadTable

# Define UI for the shiny app


ui <- basicPage(
  titlePanel("trendtracker"),
  sidebarLayout(
    sidebarPanel(
      
      # Allows you to select a test type
      
      pickerInput(
        "turnover.time", "Test Name:",
        choices = as.character(sort(unique(data.cleaned$Investigation))),
        options = list(`actions-box` = TRUE), 
        multiple = T,
        selected = as.character(sort(unique(data.cleaned$Investigation)))[1]),
      
      pickerInput(
        "bill.type", "Work Type:",
        choices = as.character(sort(unique(data.cleaned$Billing.Category))),
        options = list(`actions-box` = TRUE), 
        multiple = T,
        selected = as.character(sort(unique(data.cleaned$Billing.Category)))[1]),
      
      pickerInput(
        "referrer.name", "Referrer Name:",
        choices = as.character(sort(unique(topHospitals$topHospitals))),
        options = list(`actions-box` = TRUE), 
        multiple = T,
        selected = as.character(sort(unique(topHospitals$topHospitals)))[1]),
      
      # Allows you to select a range of dates
      
      dateRangeInput(
        "date.range", "Test Report Date Range:",
        start = sort(unique(data.cleaned$Test.Report.Produced))[1],
        end = sort(unique(
          data.cleaned$Test.Report.Produced))[length(sort(unique(
            data.cleaned$Test.Report.Produced)))]),
      
      # Download button
      
      downloadButton(
        "download.data", "Download data as .csv")
      
      ),
      
      # Main panel for plotting the graphs
      
      mainPanel(
        plotOutput(outputId = "turnoverPlot"),
        plotOutput(outputId = "testsPerYear"),
        plotOutput(outputId = "billingType"),
        plotOutput(outputId = "topReferrers"))
  )
)


# Define server logic for slider examples

server <- function(input, output) {
  
  # Drop down box inputs
  
  observe({
    print(input$turnover.time)
  })
  
  # Information on the dates selected
  
  output$date.rangeText  <- renderText({
    paste(
      collapse = " to ",
      as.character(input$date.range[1]),
      as.character(input$date.range[2])
    )
  })

  output$turnoverPlot <- renderPlot({(
    TurnoverPlot(input$turnover.time, input$date.range)
    )})
  output$testsPerYear <- renderPlot({(
    TestsPerYear(input$turnover.time, input$date.range)
    )})
  output$billingType <- renderPlot({(
    BillingType(input$bill.type, input$date.range)
    )})
  output$topReferrers <- renderPlot({(
    TopReferrers(input$referrer.name, input$date.range)
    )})
  
  output$download.data <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), "-export.csv", sep = "")
    },
    content = function(file) {
      write.csv(CreateDownloadTable(
        data.cleaned, input$date.range, input$referrer.name, input$bill.type),
        file)
    }
  )

}

shinyApp(ui = ui, server = server)
