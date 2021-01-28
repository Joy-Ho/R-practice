quote <-
  "The most valuable thing you can have as a leader is clear data"


author <-  "Ruth Porat"

paste(quote, "by", author)


paste0("I", "love")

is.character(pi)


print(paste(quote, author), quote = FALSE)
print(paste(quote, author), quote = TRUE)

cat(paste(quote, author))

cat(letters)
cat(letters, sep="-")

cat(quote, author, fill = FALSE)
cat(quote, author, fill = TRUE)


alphabet <- paste(LETTERS, collapse = "")
paste(LETTERS, collapse = "-")
paste(LETTERS)

x <- runif(10)
y <- rnorm(10, 0,1)
save(x,y,file = "xy.RData")

y<-is.finite(5>0)
is.finite(13)

ls<-list(v1=c(1,3,5), 
         rep(c(14,2,1,3),2), 
         list(c(TRUE, FALSE, TRUE),
              m2=matrix(1:9, nrow=3, ncol=3)))



ls[1] 
ls[[1]]= c(2,2,2)
ls$v1

ls$v1[2]


ls[3]
ls[[3]]


ls[[3]]$m2[2,]

ls[[3]]$m2[,2] = c(10,11,12)


library(tidyr)

table4a
gather(table4a, `1999`,`2000`, key="Year", value = "cases")


table2
spread(table2, key = type, value = count)

table3
separate(table3, rate, into = c("cases", "population"), sep = "/")

table5
unite(table5, new_year, century, year)
unite(table5, new_year, century, year, sep = "")


x <- c(1:3, NA, 5, NA)
y <- is.na(x)

log(32,2)
-log(0.7,2)*(0.7)-log(0.3,2)*(0.3)

library(tidyr)
library(readr)
library(forecast)
library(infotheo)
a<-read.csv("modified_covid_19_data.csv")

a%>% head(15)
library(magrittr)


y<- factor(c("low", "moderate", "low", "severe","low","high"),
           ordered = TRUE)


df<-data.frame(col1 = c(1:3,NA),
               col2 = c("this", "NA", "is", "text"),
               col3 = c(TRUE, FALSE,TRUE,TRUE),
               col4 = c(2.5, 4.2, 3.2, NA),
               stringsAsFactors = TRUE)
sum(is.na(df))

str(df)


x<-c(1:3, NA,5)
y<-mean(x)
y


x<-5
y<-is.finite(x>0)
y

x <- c(4,2,"TRUE", "FALSE")
class(x)





x<-c(1, NA, 2,3)
y<- mean(complete.cases(x))
y


z <- c("a", "b", "c", 1,2,4)
class(z)


library(tidyr)
table4a %>%
  gather(key = "year", value = "cases",`1999`: `2000`)



library(stringr)
set1 <- c("VIC", "NSW", "TSA", "WA","SA","TAS")
set2 <- c("SA", "WA", "NSW")
setequal(set1, set2)


x<- c(1,3,5)
y<- c(1,2,3,4,5)

setdiff(x,y)

year(Sys.time())




