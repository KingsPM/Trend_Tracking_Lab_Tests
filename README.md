trendtracking
========

### Summarising and plotting lab data to look at trends and productivity.

This [R](https://www.R-project.org/) script currently outputs four summary PDFs, "Billing Categories", "Tests Each Month", "Top 10 Referrers" and "Turnover Time Per Test". It also outputs two excel summary workbooks with the lab investigations, and their frequencies, divided into sheets based on either the month conducted ("Investigations per month per hospital") or the referring hospital ("Investigations per hospital per month").

## Running the script

To run the script nagivate to its location on your PC on the command prompt and run:

`Rscript Lab_Activity_Data.R`

You will be prompted to select an input excel file and a location you want to save the output in.

The script requires the columns "SIDC09.Hospital", "SIDC13.Investigation.Requested", "SIDC16.Date.Sample.Received", "Reported.date" and "Real.Billing.category" to be present in the input Excel file.
