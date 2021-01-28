install.packages("Hmisc")

library(readr)
library(dplyr)
library(readxl)
library(gdata)
library(rvest)
library(tidyr)
library(knitr)
library(deductive)
library(validate)
library(Hmisc)
library(stringr)

cons <- readRDS(file = "pr.RDS")

print(is.na(cons))
print(is.na(cons$Country))
print(is.na(cons$`2016`))
print(is.na(cons$`2017`))
print(is.na(cons$`2016_Q1`))
print(is.na(cons$`2016_Q2`))
print(is.na(cons$`2016_Q3`))
print(is.na(cons$`2016_Q4`))
print(is.na(cons$`2017_Q1`))
print(is.na(cons$`2017_Q2`))
print(is.na(cons$`2017_Q3`))
print(is.na(cons$`2017_Q4`))

# or
sapply(pr, function(x) print(is.na(x)))

which(is.na(cons))
which(is.na(cons$Country))
which(is.na(cons$`2016`))
which(is.na(cons$`2017`))
which(is.na(cons$`2016_Q1`))
which(is.na(cons$`2016_Q2`))
which(is.na(cons$`2016_Q3`))
which(is.na(cons$`2016_Q4`))
which(is.na(cons$`2017_Q1`))
which(is.na(cons$`2017_Q2`))
which(is.na(cons$`2017_Q3`))
which(is.na(cons$`2017_Q4`))

sum(is.na(cons))
sum(is.na(cons$Country))
sum(is.na(cons$`2016`))
sum(is.na(cons$`2017`))
sum(is.na(cons$`2016_Q1`))
sum(is.na(cons$`2016_Q2`))
sum(is.na(cons$`2016_Q3`))
sum(is.na(cons$`2016_Q4`))
sum(is.na(cons$`2017_Q1`))
sum(is.na(cons$`2017_Q2`))
sum(is.na(cons$`2017_Q3`))
sum(is.na(cons$`2017_Q4`))

colSums(is.na(cons))


## 2
complete.cases(cons)

cons[complete.cases(cons),]

cons[!complete.cases(cons),]

cons[!complete.cases(cons),"2016"]

cons[!complete.cases(cons),c("2016","2017")]

na.omit(cons)

rowSums(is.na(cons))

cons[ rowSums(is.na(cons)) > 9 ,]

cons1 <- cons[rowSums(is.na(cons)) < 10,]


# Q3
is.notanumber <- function(x){ if (is.numeric(x)) is.nan(x) }

cons <- readRDS(file = "pr.RDS")

cons$`2017_Q4` <- ifelse(is.na(cons$"2017_Q4"),
                         rowMeans(cons[,c("2017_Q1", "2017_Q2", "2017_Q3")], na.rm = TRUE),
                         cons$`2017_Q4`)


cons$`2017` <- ifelse(is.na(cons$`2017`),
                         rowMeans(cons[,c("2017_Q1", "2017_Q2", "2017_Q3","2017_Q4")],
                                  na.rm = TRUE),
                         cons$`2017`)


is.notanumber <- function(x){ if (is.numeric(x)) is.nan(x) }
is.notanumber(cons$`2017`)

sum(is.notanumber(cons$`2017`))

sapply(cons, is.notanumber)


## Q4
pop <- read_csv("popbycountry.csv")
glimpse(pop)
str(pop)

# dplyr solution
library(magrittr)
pop %<>% mutate_at(vars(starts_with("19") | starts_with("20")), .funs = as.numeric)
# or
pop %<>% mutate_at(vars(colnames(pop[, 2:32])), .funs = as.numeric)

glimpse(pop)
str(pop)


pop <- read.csv("popbycountry.csv", stringsAsFactors = FALSE)
pop[, c(2:32)] <- as.numeric(as.character(unlist(pop[, c(2:32)])))

colSums(is.na(pop))
rowSums(is.na(pop))


pop[pop == "--"] <- NA


pop2 <- pop[rowSums(is.na(pop)) < 31, ]

rowSums(is.na(pop2))


## 5
pop_ger <- pop %>% filter(X1 == "Germany")
pop_ger <- pop %>% filter(X == "Germany")

pop_ger2 <- pop %>% filter(str_detect(X1, "Germany"))
pop_ger2 <- pop %>% filter(str_detect(X, "Germany"))
# or
pop_ger3 <- pop %>% filter(X1 %in% c("Germany", "Germany, East", "Germany, West"))

str(pop_ger2)

pop_ger2[1,2]

pop_ger2[1, is.na(pop_ger2[1, 2:32])] <- t(colSums(pop_ger2[2:3, 2:32]))

t(colSums(pop_ger2[2:3, 2:32]))


## 6

pop[70, is.na(pop[70, 1:32])] <- t(colSums(pop[71:72, 2:32]))
pop <- pop %>% filter(X != c("Germany, East", "Germany, West"))
pop <- pop[rowSums(is.na(pop)) < 31, ]






############################
####### TEACHER
pop <- read.csv("popbycountry.csv", stringsAsFactors = FALSE)
pop %<>% mutate_at(vars(starts_with("X19") |  starts_with("X20")) , .funs = as.numeric)
str(pop)

pop_ger <- pop %>% filter(X %in% c("Germany", "Germany, East", "Germany, West" ))
pop_ger <- pop %>% filter(str_detect(X, "Germany"))
str(pop_ger)
pop_ger[1, is.na(pop_ger[1, 2:32])] <- colSums(pop_ger[2:3, 2:32])

