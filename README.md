# Trend_Tracking_Lab_Tests

### Summarising and plotting lab data to look at trends and productivity.

This R script currently outputs four summary PDFs, "Billing Categories", "Tests Each Month", "Top 10 Referrers" and "Turnover Time Per Test". It also outputs two excel summary workbooks with the lab investigations, and their frequencies, divided into sheets based on either the month conducted ("Investigations per month per hospital") or the referring hospital ("Investigations per hospital per month").

you will have to install java environments locally outside of R to get libraries such as xlsx to work, if they are not present already. On a Linux system this is done with:

`sudo apt install default-jre`
`sudo apt install default-jdk`
`R CMD javareconf`

To run the script nagivate to the file location and run:

`Rscript Lab_Activity_Data.R`

You will be prompted to select an input excel file and a location you want to save the output in.
