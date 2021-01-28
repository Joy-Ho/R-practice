library(readr)
library(xlsx)
library(readxl)
library(foreign)
library(gdata)
library(rvest)

install.packages(c("readr", "xlsx", "readxl", "foreign", "gdata", "rvest"))

## Question 1 ---------------------------------------
getwd()

## Question 2 ---------------------------------------
setwd("C:/Users/ericn/OneDrive/Documents")
setwd("C:/Users/ericn/OneDrive/Documents/week2")

## Question 3 ---------------------------------------
population <- read.csv("population.csv")
#or
population <- read.csv("C:/Users/ericn/OneDrive/Documents/population.csv")

head(population)
head(population, 10)

# Question 4 ---------------------------------------
population <- read_csv("population.csv")
head(population)

# Question 5 ---------------------------------------
population <- read.spss("population.sav")
population2 <- read.spss("population.sav", to.data.frame = TRUE)

head(population)

## Question 6 --------------------------------------
population1 <- read_excel("population-migration.xls", sheet = 1, skip = 1)
population2 <- read_excel("population-migration.xls", sheet = 2, skip = 1)

# use sheet names
population3 <- read_excel("population-migration.xls", sheet = "Population Density and Regional", skip = 1)
# is equal to population1

population3 <- read_excel("population-migration.xls", sheet = "Inter-regional Migration", skip = 1)
# is equal to population2


## Question 7 ---------------------------------------
url <- "https://data.cityofnewyork.us/api/views/25th-nujf/rows.csv?accessType=DOWNLOAD"

baby_names <- read.csv(url)
#or
baby_names <- read_csv(url)

head(baby_names, 5)
tail(baby_names)


## Question 8 --------------------------------------
write.csv(baby_names, "output/PopularBabyNames_base.csv")
#or
write_csv(baby_names, "output/PopularBabyNames_readr.csv")

## Question 9
save(baby_names, file="output/PopularBabyNames.Rdata")


## Question 10
aus_cities <- read_html("https://en.wikipedia.org/wiki/List_of_cities_in_Australia_by_population")

# find table tag
alltables <- html_nodes(aus_cities,"table")

alltables

aus_cities <- html_table(alltables[[1]])

write.table(aus_cities, "output/aus_cities.csv")
#There are 9 tables. Figure out what the table is
# right click in the website -> inspect elements -> search " <table "

## Question 11
# Import data -> From Text (readr) ...
# or
auscities <- read_delim("aus.txt",
                        delim = " ", col_names = FALSE,
                        trim_ws = TRUE, skip = 1)








