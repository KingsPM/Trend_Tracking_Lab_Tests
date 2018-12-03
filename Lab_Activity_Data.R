#!/usr/bin/env Rscript

# TODO(Callum):
#   Make the script more portable (currently need java prerequisites)

# have to install java environments locally outside of R
# sudo apt install default-jre
# sudo apt install default-jdk
# R CMD javareconf

##### Install packages, load libraries, get file paths #####

# Function to install packages (if not installed already) and load them


getPackages <- function(required.packages) {
  packages.not.installed <- 
    required.packages[!(required.packages %in% installed.packages()[, "Package"])]
  if(length(packages.not.installed)){
    install.packages(packages.not.installed)}
  lapply(required.packages, require, character.only = TRUE)
}


# tidyverse had issues loading in Ubuntu 18.04.1 LTS so I removed it.
# tidyverse loads ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats.

getPackages(c("tcltk", "readxl", "reshape2", "lubridate", "xts", "data.table", "ggplot2",
              "gridExtra", "xlsx", "rJava", "xlsxjars",  "openxlsx",  "openxlsx"))

input.excel <- tk_choose.files(default = '',
                               caption = "Please select the input excel file")

out.dir <- tk_choose.dir(default = getwd(),
                         caption = "Where should the output be saved?")

# Test that there is at least one argument, if not throw an error.

if (length(input.excel) == 0) {
  stop("You need to add an input Excel file and a output location",
       call. = FALSE)
}

data <- read_excel(input.excel)  # Get data frame from input excel file

##### Produce a table that has the turnaround times from the input excel #####


createDF <- function(data) {
  Audit <- data[c("SIDC09 Hospital", "SIDC13 Investigation Requested", "SIDC16 Date Sample Received", "Reported date", "Real Billing category")]
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


# If you want to only count weekdays can use the line below (don't do this currently)
# sum(!grepl("S", weekdays(seq(Sys.Date(), as.Date(scan(,""), "%d.%m.%Y"), 1)))) + 1

##### Plot turnover time of each test per month #####


createPDF1 <- function(data) {
  turnoverTime <- data[c("Investigation", "Year_Month", "Turnover_Time", "Target_Turnaround")]
  turnoverTime <- na.omit(turnoverTime)
  pdf(paste0(out.dir, "/Turnover Time Per Test.pdf"))
  sort(unique(turnoverTime$Investigation))
  lapply(sort(unique(turnoverTime$Investigation)), 
         function(i) {ggplot(turnoverTime[turnoverTime$Investigation == i,],
                             aes(x = Year_Month, y = Turnover_Time)) +
             labs(title = i, subtitle = "Outliers, defined as ±1.5*IQR, output
                  have not been plotted. Boxplots \nrepresent Median, Q1-Q3 
                  and 1.5*Q1Q3. Red line is the mean.",
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
                        color = "red", size = .5)}
         )
  dev.off()
}


##### Print out some excel sheets #####

# Create dataset
# Save test frequency and referring hospital for each year 


createExcel1 <- function(data) {
  wb = createWorkbook(paste0(out.dir, "/Investigations per hospital per month"))
  Hospital <- data[c("Hospital", "Investigation", "Year_Month")]
  for (i in sort(unique(Hospital$Year_Month))){
    excelPrintout <- Hospital[Hospital$Year_Month == i,]
    excelPrintout <- excelPrintout[c("Hospital", "Investigation")]
    excelPrintout <- as.data.frame(table(excelPrintout))
    row_sub <- excelPrintout$Freq != 0
    excelPrintout <- excelPrintout[row_sub,]
    addWorksheet(wb = wb, sheetName = i)  # Doesn't print?
    writeData(wb = wb, sheet = i, x = excelPrintout)
    saveWorkbook(wb, paste0(out.dir, "/Investigations per hospital per month.xlsx"),
               overwrite = TRUE)}
}


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

##### Plot number of investigations per month, year by year #####

# Count the number of results for each investigation. Convert investigations into 
# a frequency while retaining year and month as a subcategory.


createPDF2 <- function(data) {
  Investigation <- data[c("Investigation", "Year_Reported", "Month_Reported")]
  Investigation <- Investigation[!(Investigation$Year_Reported == "1899"),]  # rm anomalous data
  Investigation <- as.data.frame(table(Investigation))
  Investigation[Investigation == 0] <- NA
  
  # Plot line for number of investigations done each month, year on year
  
  pdf(paste0(out.dir, "/Tests Each Month.pdf"))
  
  lapply(sort(unique(Investigation$Investigation)), 
    function(i) {ggplot(Investigation[Investigation$Investigation == i,], 
      aes(Month_Reported, Freq, group = Year_Reported, color = Year_Reported)) +
    labs(title = i, y = "Frequency", x = "Month") + geom_point() +
    geom_line() + theme_minimal() + theme(axis.text.x = element_text(),
                                          panel.grid.major.x = element_blank(),
                                          panel.grid.major.y = element_line(size = .1, color = "black"))
      })

dev.off()
``
}


##### Number of billing types per month, year on year #####


createPDF3 <- function(data) {
  workFreq <- data[c("Billing_Category", "Year_Reported", "Month_Reported")]
  workFreq <- as.data.frame(table(workFreq))
  workFreq[workFreq == 0] <- NA
  pdf(paste0(out.dir, "/Billing Categories.pdf"))
  lapply(sort(unique(workFreq$Billing_Category)), 
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

dev.off()

}


createPDF4 <- function(data) {
  topHospitals <- data[c("Hospital")]
  topHospitals <- as.data.frame(table(topHospitals))
  topHospitals <- topHospitals[ave(-topHospitals$Freq, FUN = rank) <= 10, ]
  referrerHospital <- data[c("Hospital", "Year_Reported", "Month_Reported")]
  referrerHospital <- as.data.frame(table(referrerHospital))
  referrerHospital <- subset(referrerHospital, Hospital %in% topHospitals$topHospitals)
  referrerHospital[referrerHospital == 0] <- NA
  pdf(paste0(out.dir, "/Top 10 Referrers.pdf"))
  lapply(sort(unique(referrerHospital$Hospital)),
         function(i) {ggplot(referrerHospital[referrerHospital$Hospital == i,],
                             aes(Month_Reported, Freq, group = Year_Reported, color = Year_Reported)) +
             labs(title = i, y = "Frequency", x = "Month") + geom_point() +
             geom_line() + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                                                   panel.grid.major.x = element_blank(),
                                                   panel.grid.major.y = element_line(size = .1, color = "black"))
})

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


if (!interactive()) {
  main(createDF(data))
}

# main(createDF(data))
