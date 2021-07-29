# Get a CSV and store it
csv_select <- function(csvname) {
    switch(csvname,
         "chelems" = db <- read.csv(file = "csv/chelems.csv", header = TRUE, sep = ";"),
         "ereopen" = db <- read.csv(file = "csv/ere open 3fix.csv", header = TRUE, sep = ";")
    )
}