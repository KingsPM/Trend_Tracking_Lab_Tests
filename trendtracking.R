#!/usr/bin/env Rscript

# TODO(Callum):
#   The functions keeps dropping rows containing "non-finite" values, fix this.

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
              "ggplot2", "gridExtra", "openxlsx"))

input.excel <- tk_choose.files(default = '',
                               caption = "Please select the input excel file")

out.dir <- tk_choose.dir(default = getwd(),
                         caption = "Where should the output be saved?")

# Test that there is at least one argument, if not throw an error.

if (length(input.excel) == 0) {
  stop("You need to add an input Excel file and a output location",
       call. = FALSE)
}

data <- read.xlsx(input.excel)  # Get data frame from input excel file

##### Produce a table that has the turnaround times from the input excel #####


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


##### Print out some excel sheets #####

# Print out the number of tests per referring hospital, one worksheet for each month 


createExcel1 <- function(data) {
    wb = createWorkbook(paste0(out.dir, "/Investigations per hospital per month"))
    Hospital <- data[c("Hospital", "Investigation", "Year_Month")]
    for (i in sort(unique(Hospital$Year_Month))){
        excel.printout <- Hospital[Hospital$Year_Month == i,]
        excel.printout <- excel.printout[c("Hospital", "Investigation")]
        excel.printout <- as.data.frame(table(excel.printout))
        row_sub <- excel.printout$Freq != 0
        excel.printout <- excel.printout[row_sub,]
        addWorksheet(wb = wb, sheetName = i)  # Doesn't print?
        writeData(wb = wb, sheet = i, x = excel.printout)
        saveWorkbook(wb, paste0(out.dir, "/Investigations per hospital per month.xlsx"),
            overwrite = TRUE)}
}


# Print out the number of tests per referring hospital per month, one worksheet for test type.


createExcel2 <- function(data) {
    wb = createWorkbook(paste0(out.dir, "/Investigations Each Month By Hospital"))
    Hospital <- data[c("Hospital", "Investigation", "Year_Month")]
    for (i in sort(unique(Hospital$Investigation))){
        excelPrintout <- Hospital[Hospital$Investigation == i,]
        excelPrintout <- excelPrintout[c("Hospital", "Year_Month")]
        excelPrintout <- as.data.frame(table(excelPrintout))
        row_sub <- excelPrintout$Freq != 0
        excelPrintout <- excelPrintout[row_sub,]
        addWorksheet(wb = wb, sheetName = substr(i, 1, 31))  # Needed to substring as name limit of 31 characters
        writeData(wb = wb, sheet = substr(i, 1, 31), x = excelPrintout)
        saveWorkbook(wb, paste0(out.dir, "/Investigations per month per hospital.xlsx"),
            overwrite = TRUE)}
}


###############################################################################

##### Plot turnover time of each test per month #####


createPDF1 <- function(data) {
    turnoverTime <- data[c("Investigation", "Year_Month", "Turnover_Time", "Target_Turnaround")]
    turnoverTime <- na.omit(turnoverTime)
    pdf(paste0(out.dir, "/Turnover Time Per Test.pdf"))
    graph <- lapply(sort(unique(turnoverTime$Investigation)), 
        function(i) {ggplot(turnoverTime[turnoverTime$Investigation == i,],
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
                color = "red", size = .5)})
    print(graph)
    dev.off()
}


##### Plot number of investigations per month, year by year #####

# Count the number of results for each investigation. Convert investigations into 
# a frequency while retaining year and month as a subcategory.


createPDF2 <- function(data) {
    Investigation <- data[c("Investigation", "Year_Reported", "Month_Reported")]
    Investigation <- Investigation[!(Investigation$Year_Reported == "1899"),]  # rm anomalous data
    Investigation <- as.data.frame(table(Investigation))
    Investigation[Investigation == 0] <- NA
    pdf(paste0(out.dir, "/Tests Each Month.pdf"))
    graph <- lapply(sort(unique(Investigation$Investigation)),
        function(i) {ggplot(Investigation[Investigation$Investigation == i,], 
            aes(Month_Reported, Freq, group = Year_Reported, color = Year_Reported)) +
            labs(title = i, y = "Frequency", x = "Month") + geom_point() +
            geom_line() +
            theme_minimal() +
            theme(axis.text.x = element_text(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(size = .1, color = "black"))
          })
    print(graph)
    dev.off()
}


##### Plot number of billing types per month, year on year #####


createPDF3 <- function(data) {
    workFreq <- data[c("Billing_Category", "Year_Reported", "Month_Reported")]
    workFreq <- as.data.frame(table(workFreq))
    workFreq[workFreq == 0] <- NA
    pdf(paste0(out.dir, "/Billing Categories.pdf"))
    graph <- lapply(sort(unique(workFreq$Billing_Category)), 
        function(i) {ggplot(workFreq[workFreq$Billing_Category == i,], 
        aes(Month_Reported, Freq, group = Year_Reported, color = Year_Reported)) +
        labs(title = i,
            y = "Frequency",
            x = "Month") +
        geom_point() +
        geom_line() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = .1, color = "black"))
    })
    print(graph)
    dev.off()
}


##### Plot the trends in the tests ordered by the top 10 referrer hospitals #####


createPDF4 <- function(data) {
    topHospitals <- data[c("Hospital")]
    topHospitals <- as.data.frame(table(topHospitals))
    topHospitals <- topHospitals[ave(-topHospitals$Freq, FUN = rank) <= 10, ]
    referrerHospital <- data[c("Hospital", "Year_Reported", "Month_Reported")]
    referrerHospital <- as.data.frame(table(referrerHospital))
    referrerHospital <- subset(referrerHospital, Hospital %in% topHospitals$topHospitals)
    referrerHospital[referrerHospital == 0] <- NA
    pdf(paste0(out.dir, "/Top 10 Referrers.pdf"))
    graph <- lapply(sort(unique(referrerHospital$Hospital)),
        function(i) {ggplot(referrerHospital[referrerHospital$Hospital == i,],
            aes(Month_Reported, Freq, group = Year_Reported, color = Year_Reported)) +
            labs(title = i, y = "Frequency", x = "Month") +
            geom_point() +
            geom_line() +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(size = .1, color = "black"))
          })
    print(graph)
    dev.off()
}


##### The main function to call everything #####


main <- function(input) {
    createPDF1(input)
    createPDF2(input)
    createPDF3(input)
    createPDF4(input)
    createExcel1(input)
    createExcel2(input)
}


if (!interactive()) {  # If interactive, run the fucntion main(createDF(data))
    main(createDF(data))
}

library(shiny)

runApp(appDir = "~/Documents/Trend_Tracking_Lab_Tests/shinyapp.R")
