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


createDF <- function(data) {
    data <- data[c("SIDC09.Hospital", "SIDC13.Investigation.Requested", "SIDC16.Date.Sample.Received", "Reported.date", "Real.Billing.category")]
    colnames(data) <- c("Hospital", "Investigation", "Sample_Received", "Test_Reported", "Billing_Category")
    data$Test_Reported <- as.Date(data$Test_Reported)
    data$Sample_Received <- as.Date(data$Sample_Received)
    data$Turnover_Time <- data$Test_Reported - data$Sample_Received
    data$Turnover_Time <- as.numeric(data$Turnover_Time)
    data$Year_Reported <- format(as.Date(data$Test_Reported, format="%d/%m/%Y"),"%Y")
    data$Month_Reported <- format(as.Date(data$Test_Reported, format="%d/%m/%Y"),"%m")
    data$Day_Reported <- format(as.Date(data$Test_Reported, format="%d/%m/%Y"),"%d")
    data[9] <- paste("Y", data$Year_Reported, "_M", data$Month_Reported, sep="")
    colnames(data)[9] <- "Year_Month"
    data[10] <- (30)
    colnames(data)[10] <- "Target_Turnaround"
    data <- na.omit(data)
    return(data)
}


data <- read.xlsx(input.excel)  # Get data frame from input excel file
data <- createDF(data)

#lapply(sort(unique(turnoverTime$Investigation)), 

graph <- function(i) {
  ggplot(turnoverTime[turnoverTime$Investigation == i,],
    aes(x = Year_Month, y = Turnover_Time)) +
  labs(title = i, subtitle = "Outliers, defined as ±1.5*IQR, output have not been plotted. Boxplots \nrepresent Median, Q1-Q3 and 1.5*Q1Q3. Red line is the mean.",
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
    ylim(-5, quantile(turnoverTime[turnoverTime$Investigation == i,][[3]])[[4]]+10) +
      geom_hline(yintercept = data$Target_Turnaround, linetype = "dashed",
      color = "red", size = .5)
  }

# Define UI for slider app

ui <- fluidPage(
  titlePanel("trendtracker"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("somenumber", "Slidey slide bar",
                  min = 0, max = 10,
                  value = 0),
      selectInput("test", "Test Name:",
                  c(sort(unique(turnoverTime$Investigation))))
    ),
    
    mainPanel(
      verbatimTextOutput(outputId = "printTest"),
      tableOutput("values"),
      plotOutput(outputId = "testPlot")
    )
  )
)

# Define server logic for slider examples

server <- function(input, output) {
    sliderValues <- reactive({
        data.frame(Name = c(input$test), # "Important Value"),
            Value = as.character(c(input$somenumber, stringsAsFactors = FALSE)))})
    
    # Show the values in an HTML table
    output$textOutput <- renderPrint({(input$test)})
    output$values <- renderTable({sliderValues()}) #[1,])  # Adding an extra blank row so force removed
    output$testPlot <- renderPlot({plot(graph(input$test))})
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
