library(ggplot2)

url = "C:/Users/jayakeshpk1/Dropbox/Comp/Bowery/Analytics/Blogs/NYC Open Data/Restuarants/06282017/DOHMH_New_York_City_Restaurant_Inspection_Results.csv";
url1 = "C:/Users/jayakeshpk1/Dropbox/Comp/Bowery/Analytics/Blogs/NYC Open Data/Restuarants/06282017/DOHMH_New_York_City_Restaurant_Inspection_Results.xlsx";
url2 = "C:/Users/jayakeshpk1/Dropbox/Comp/Bowery/Analytics/Blogs/NYC Open Data/Restuarants/06282017/NYCRest.csv";

restDf = read.csv("C:/Users/jayakeshpk1/Dropbox/Comp/Bowery/Analytics/Blogs/NYC Open Data/Restuarants/06282017/DOHMH_New_York_City_Restaurant_Inspection_Results.csv",
    quote = "", row.names = NULL, stringsAsFactors = FALSE, sep = '\t')

restDf = read.csv(url2,
    quote = "", row.names = NULL, stringsAsFactors = FALSE, sep = ',')

df <- read.table(url, sep = '\t', header = TRUE, fileEncoding = "UTF-16LE")

View(restDf)
nrow(restDf)

install.packages("OData")
library(OData)
df = retrieveData("https://data.cityofnewyork.us/OData.svc/xx67-kt59")

install.packages("xlsx")
library(xlsx)
df = read.xlsx(url1, sheetName = "Sheet1")

bigfile.sample <- read.csv(url2,
                           stringsAsFactors = FALSE, header = T, nrows = 20)

bigfile.colclass <- sapply(bigfile.sample, class)

library(dplyr)

bigfile.raw <- tbl_df(read.csv(url2,
                    stringsAsFactors = FALSE, header = T, nrow = 10000,
                    colClasses = bigfile.colclass, comment.char = "", quote = ""))

bigfile.raw <- read.csv(url2,
                    stringsAsFactors = FALSE, header = T, nrow = 10000,
                    colClasses = bigfile.colclass, comment.char = "", quote = "", sep = "\t")

View(bigfile.raw)


df1 = downloadResourceCsv()
library(RJSONIO)
install.packages("readxl")
library(readxl)
df = read_excel(url1)
View(df)
