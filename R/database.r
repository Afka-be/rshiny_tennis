#' Access CSV data and store it
#' @param csvname CSV file's name
csv_select <- function(csvname) {
    switch(csvname,
         "chelems" = db <- read.csv(file = "csv/chelems.csv", header = TRUE, sep = ";"),
         "ereopen" = db <- read.csv(file = "csv/ere open 3fix.csv", header = TRUE, sep = ";")
    )
}

#' Access XLSX data and store it
#' @param xlsname xlsx file's name
#' @param sheetname Name of the used sheet for storing Data
#' 
xlsx_select <- function(xlsname, sheetname) {

        switch(xlsname,
            "grandslams_unit" = db <- read_excel("csv/Grand Slams.xlsx", sheet = sheetname),
            "grandslams_total" = db <- read_excel("csv/Grand Slams Stack.xlsx", sheet = sheetname),
            "masters1000_unit" = db <- read_excel("csv/Masters1000.xlsx", sheet = sheetname),
            "masters1000_total" = db <- read_excel("csv/MastersStack.xlsx", sheet = sheetname),
            "olympics" = db <- read_excel("csv/JO.xlsx", sheet = sheetname),
            "weeksnr1" = db <- read_excel("csv/Weeksnr1.xlsx", sheet = sheetname),
            "daviscups_total" = db <- read_excel("csv/DavisStack.xlsx", sheet = sheetname)
        )

}
