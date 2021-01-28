library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(magrittr)

######################################################3
##1

#Check out the classes of each variable  use ordered argument then check its¡¦ levels.

family_incidents <- read_csv("Rweek4classworksheet/family_incidents.csv")

class(family_incidents$`Police region`)  #char
class(family_incidents$`Local government area`) # char
class(family_incidents$`2012-13`) #numeric

family_incidents %>% View()

str(family_incidents)

glimpse(family_incidents)


# convert `Police Region` column into factor,
family_incidents$`Police region` <- factor(family_incidents$`Police region`)
levels(family_incidents$`Police region`)

family_incidents$`Police region` <- factor(family_incidents$`Police region`,
                                           levels = c("North West Metro", "Eastern", "Southern Metro",
                                                      "Western", "Total2"))
levels(family_incidents$`Police region`)

# check 
class(family_incidents$`Police region`)


#########################################################################################
## 2

# Answer: b


##########################################################
## 3
#  Tidy the family_incidents data set into the form below.

fam <- family_incidents %>%
  gather(`2012-13`, `2013-14`, `2014-15`, `2015-16`, `2016-17`,
         key = "year", value = "cases")

# same way
fam <- family_incidents %>%
  gather(`2012-13`:`2016-17`,
         key = "year", value = "cases")

# same way
fam <- family_incidents %>%
  gather(3:7,
         key = "year", value = "cases")

head(fam, 4)


#######################################################################
## 4

#  change the separator of the year column
#  from "-" to "/" using separate() and unite() functions

fam %>%
  separate(`year`, into = c("Start", "End"), sep = "-") %>%

fam %>%
  separate(`year`, into = c("Start", "End"), sep = "-") %>%
  unite(`Year`, Start, End, sep = "/")


