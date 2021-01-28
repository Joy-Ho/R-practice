library(dplyr)
library(readr)
library(tidyr)
library(knitr)


## Excercise 1 --------------------------------------
## Use command c() to create vectors as listed below and check their class as you go. 
## For factor class, check it’s levels and label it.
## i) Integers from 1 to 5 and name it vect_int.
vect_int <- c(1L:5L)
# or
vect_int <- c(1L, 2L, 3L, 4L, 5L)
class(vect_int)
is.character(vect_int)

## ii) Double numeric variables from 0.5 to 3.5 incrementing it 1, and name it vect_dbl.
vect_dbl <- c(0.5:3.5)
typeof(vect_dbl)

## iii) Character variables using name of the colours red, green, blue, yellow, white and name it vect_char.
vect_char <- c("red", "green", "blue","yellow", "white")
class(vect_char)
is.numeric(vect_char)
is.character(vect_char)

## iv) 
vect_fact <- factor(c("very low", "low", "medium", "high", "very high"))
levels(vect_fact)

class(vect_fact)
typeof(vect_fact)




vect_fact2 <- factor(c("very low", "low", "medium", "high", "very high"), 
                levels = c("very low", "low", "medium", "high", "very high"),
                labels = c("very low", "low", "medium", "high", "very high"))
levels(vect_fact)

## Excercise 2 --------------------------------------
vect_fact3 <- factor(c("very low", "low", "medium", "high", "very high"), 
                      levels = c("very low", "low", "medium", "high", "very high"),
                      labels = c("very low", "low", "medium", "high", "very high"),
                       ordered = TRUE)
vect_fact3
levels(vect_fact3)

## Excercise 3 --------------------------------------
vect_comb <- c(vect_int, vect_fact3)
class(vect_comb)

## Excercise 4 --------------------------------------
vect_list <-list(vect_int, vect_char, vect_comb, vect_dbl,
                 vect_fact,vect_fact2,vect_fact3)
str(vect_list)


vect_list2 <- append(vect_list, list(c("ACT", "VIC", "NSW", "QLD", 
                                      "NT", "SA", "TAS","WA")))
str(vect_list2)  


names(vect_list2) <- c("comp1", "comp2", "comp3", "comp4",
                       "comp5", "comp6", "comp7", "comp8")

# i) Select the third element of comp5.
vect_list2$comp5[3]

# ii) Select the second, fourth and eighth component of the list all together.
vect_list2[c(2,4,8)]

## Excercise 5 --------------------------------------
matrix(seq(0,36, by = 2), nrow = 5, ncol = 4)
#The length of the data and the size of the matrix
#aren't equal so R recycles the data and it starts
#from the beginning of the sequence

mat1 <-matrix(seq(0,36, by = 2), nrow = 5, ncol = 4)
str(mat1)
attributes(mat1)

## Excercise 6 --------------------------------------
m1 <- rbind(vect_char, vect_fact3)
m2 <- cbind(vect_char, vect_fact3)
mat2 <- cbind(mat1, m2)
attributes(mat2)
structure(mat2)


## Excercise 7 --------------------------------------
m3 <- cbind(1:4, vect_dbl)

m4 <- cbind(m2, m3)
# m3 and m4 doesn't have the same number of rows.
#This logic applies if number of columns aren't the same

## Excercise 8 --------------------------------------
colnames(mat2) <- c("seq1", "seq2", "seq3", "seq4", "colours", "factor1")
rownames(mat2) <- c("x1", "x2", "x3", "x4", "x5")
attributes(mat2)

## Excercise 9 --------------------------------------
df1 <- data.frame(col1 = vect_int,
                  col2 = vect_char,
                  col3 = vect_fact3)
str(df1)

# df2 <- data.frame(col1 = vect_int,
#                  col2 = vect_char,
#                   stringsAsFactors = False)
# str(df2)
# Ctrl + Shift + C


## Excercise 10 --------------------------------------
df3 <- cbind(df1, vect_fact3)
str(df3)


## Excercise 11 --------------------------------------
colnames(df3) <- c("numbers", "colours", "scale")
rownames(df3) <- c( "r1", "r2", "r3", "r4", "r5")


## Excercise 12 --------------------------------------
df3[4:5,]
df3[c("r4","r5"),]

df3[, c(1,3)]
df3[, c("numbers", "scale")]

df3$scale

## Excercise 13 --------------------------------------
## Convert df3’s columns using as.
# i) 

# ii)

# iii)
str(df3)

df3$numbers <- as.numeric(df3$numbers)
str(df3)
class(df3$numbers)

df3$colours <- as.character(df3$colours)
str(df3)

df3$scale <- as.character(df3$scale)


## Excercise 14 --------------------------------------

df5 <- as.data.frame(mat2)
is.data.frame(df5)
str(df5)

mat4 <- as.matrix(df3)
is.matrix(mat4)
str(mat4)


## Excercise 15 --------------------------------------
library(readr)
germangss <- read_csv("germangss.csv")

str(germangss)

colnames(germangss) <- c("Political Attitude",
                         "Age Category", "Year",
                         "Education Level", "Region",
                         "Binary Class")

class(germangss$`Political Attitude`)
class(germangss$`Age Category`)
class(germangss$Year)
class(germangss$`Education Level`)
class(germangss$Region)
class(germangss$`Binary Class`)

germangss$`Political Attitude` <- factor(germangss$`Political Attitude`,
                                         levels = c("very_poorly", "poorly",
                                                   "well", "very_well"),
                                         ordered = TRUE)

germangss$`Age Category`<- factor(germangss$`Age Category`,
                                  levels = c("19-29", "30-44", "45-59",
                                             "60-74", "75+"),
                                  ordered = TRUE)

germangss$Year <- as.numeric(germangss$Year)

subgss <- as.data.frame(germangss[1:30,c("Political Attitude",
                                         "Age Category",
                                         "Education Level")])
is.data.frame(subgss)



