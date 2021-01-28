library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(magrittr)
library(knitr)


####################################
## 1
family_incidents <- read_csv("Rweek5classworksheet/family_incidents.csv")


# select only 2015-2016 and 2016-2017
family_incidents %>% select("2015-16", "2016-17")     # select "2015-16" and "2016-17"
# or
family_incidents %>% select(-("2013-14":"2016-17"))



family_incidents %>% select(-("2015-16": "2016-17"))  # select everything but do not select "2015-16" to "2016-17"
family_incidents %>% select(-c("2015-16", "2016-17"))
family_incidents %>% select(-"2015-16", -"2016-17")
family_incidents %>% select(contains("13"))           # select columns that contains "13"


# group by police region
# show a summary of mean and standard deviation of 2015-2016 and 2016-2017
family_incidents%>% 
  select(`2015-16`,`2016-17`) %>%
  group_by(family_incidents$`Police region`) %>%
  summarise(mean1=mean(`2015-16`),
            mean2=mean(`2016-17`),
            sd1=sd(`2015-16`),
            sd2=sd(`2016-17`))

################################################################################
# Q1 Another way
family_incidents2 <- read_csv("Rweek5classworksheet/family_incidents.csv", 
                              col_names = c("police_region", 
                                            "lga",
                                            "2012-13",
                                            "2013-14",              
                                            "2014-15" ,
                                            "2015-16",
                                            "2016-17"), skip = 1)

names(family_incidents2)

family_incidents2 %>%
  select(police_region, "2015-16","2016-17") %>%
  group_by(police_region) %>%
  summarise(mean1=mean(`2015-16`),
            mean2=mean(`2016-17`),
            sd1=sd(`2015-16`),
            sd2=sd(`2016-17`))


# filter
family_incidents2 %>%
  select(police_region, "2015-16","2016-17") %>%
  filter(!police_region %in% "Total2") %>%
  group_by(police_region) %>%
  summarise(mean1=mean(`2015-16`),
            mean2=mean(`2016-17`),
            sd1=sd(`2015-16`),
            sd2=sd(`2016-17`))

######################################################
## 2

influenza <- read_excel("Rweek5classworksheet/Influenza.xlsx",
                        sheet = "Flu Public Dataset",skip = 1)
levels(influenza$State)
unique(influenza$State)


influenza$State <- 
  factor(
    influenza$State , 
    levels=c("NSW", "NT", "Qld", "SA", "Tas", "Vic", "WA"),
    labels=c("NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")
    )
levels(influenza$State)
unique(influenza$State)

# or
influenza %<>%
  mutate(State = factor(State, 
                        levels=c("NSW", "NT", "Qld", "SA", "Tas", "Vic", "WA"),
                        labels=c("NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")
                        )
         )


influenza$`Age  group`<- 
  factor(
    influenza$`Age  group`, 
    levels=c("00-04", "05-09", "10-14", "15-19", "20-24" ,"25-29" ,"30-34",
             "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
             "70-74", "75-79", "80-84", "85+", "Unknown"),
    ordered = TRUE)
levels(influenza$`Age  group`)
unique(influenza$`Age  group`)


influenza %>%
  filter(`Indigenous status` == 'Indigenous') %>%
  group_by(`Age  group`, Sex, State) %>%
  summarise(count_sex = n()) %>%
  arrange("Age group")


influenza %>%
  filter(`Indigenous status` == 'Indigenous') %>%
  group_by(`Age  group`, Sex, State) %>%
  summarise(count_sex = n()) %>%
  arrange(count_sex)

influenza %>%
  filter(`Indigenous status` == 'Indigenous') %>%
  group_by(`Age  group`, Sex, State) %>%
  summarise(count_sex = n()) %>%
  arrange(desc(count_sex))


##############################################
## Q3
df1 <-  
  influenza %>% 
  filter(`Indigenous status` == 'Indigenous') %>%
  mutate(year = substr(`Week Ending (Friday)`, 1, 4)) %>%
  group_by(year, State) %>%
  summarize(cases = n()) %>%
  ungroup() %>%
  spread(key = State, value = cases)


df1



##############################################################
## Q4
df2 <- influenza %>%
  mutate(year = substr(`Week Ending (Friday)`, 1, 4)) %>%
  group_by(year, State) %>%
  summarize(cases = n()) %>% 
  spread(key = State, value = cases)

left_join(df1, df2, by = ("year"))

join0 <- left_join(df1, df2, by = ("year"), suffix = c("_I", ""))

join0 <- df1 %>% 
  left_join(df2, by = ("year"), suffix = c("_I", "")) 

df1
df2
join0


###################################################################
## Q5

vic_pet <- read_csv("Rweek5classworksheet/VIC_pet.csv")

sa_pet <- read_csv("Rweek5classworksheet/SA_pet.csv")

bind_rows(vic_pet, sa_pet)

x <- vic_pet %>% bind_rows(sa_pet)
x<-bind_rows(vic_pet, sa_pet)


# base R- rbind(), cbind()

# set operations 

y <- union(vic_pet, sa_pet)

z <- intersect(x, y)

t <- setdiff(x, y)

dim(vic_pet)
dim(sa_pet)
dim(x)



######## example ########
ex1 <- c(0, 1, 2)
ex2 <- c(2, 3, 4)
setdiff(ex1, ex2)
setdiff(ex2, ex1)
union(ex1, ex2)

rbind(ex1, ex2)
cbind(ex1, ex2)
########################################################
## Q6

pet1 <- read.csv("Rweek5classworksheet/pet1.csv")
pet2 <- read.csv("Rweek5classworksheet/pet2.csv")
pet3 <- read.csv("Rweek5classworksheet/pet3.csv")

# left_join
join11 <- left_join(pet1, pet2)
join12 <- left_join(join11, pet3)

pet_left <-  pet1 %>%
  left_join(pet2) %>%
  left_join(pet3)


# right_join
join13 <- right_join(pet1, pet2)
pet_join <- right_join(join13, pet3)

pet_right <- pet1 %>% 
  right_join(pet2) %>% 
  right_join(pet3)



join21 <- setdiff(pet_right, pet_left)

anti_pet <- anti_join(pet_right, pet_left, by = "id")

#######################################################
# 7. Use a suitable join function to join `pet2`
# and `pet3` datasets, only keep the rows that exists in the both datasets.

# inner join 


join3 <- inner_join(pet2, pet3, by = "id")

join3 <- pet2 %>% 
  inner_join(pet3, by = "id") 

##########################################################
# semi join 

join4 <- semi_join(pet2, pet3, by = "id")





