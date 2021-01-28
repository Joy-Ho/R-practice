library(readr)
library(dplyr)
library(outliers)
library(MVN)

cap <- function(x){
  quantiles <- quantile( x, c(.05, 0.25, 0.75, .95 ) , na.rm = T)
  x[ x < quantiles[2] - 1.5*IQR(x, na.rm = T) ] <- quantiles[1]
  x[ x > quantiles[3] + 1.5*IQR(x, na.rm = T) ] <- quantiles[4]
  x
}



###############################################################################
#Q1.
wilt1<- read_csv("C:/Users/ericn/OneDrive/Documents/training.csv")
wilt2<- read_csv("testing.csv")
wilt<- wilt1 %>% bind_rows(wilt2)

###############################################################################
#Q2.

wilt$Mean_Green %>% boxplot(main = "Box Plot of Mean Green Value", ylab="Mean (G)")
wilt$Mean_Red %>% boxplot(main = "Box Plot of Mean Red Value", ylab="Mean (R)")
wilt$Mean_NIR %>% boxplot(main = "Box Plot of Mean near infrared (NIR) value", ylab="Mean (NIR)")
wilt$GLCM_pan %>% boxplot(main = "Box Plot of Mean gray level co-occurrence matrix (GLCM) texture index", ylab="Mean (GLCM)")
###############################################################################
#Q3.

z.scores <- wilt$Mean_Green %>%  scores(type = "z")
z.scores %>% summary()
which( abs(z.scores) >3 )
length (which( abs(z.scores) >3 ))

z.scores <- wilt$Mean_Red %>%  scores(type = "z")
z.scores %>% summary()
which( abs(z.scores) >3 )
length (which( abs(z.scores) >3 ))

z.scores <- wilt$Mean_NIR %>%  scores(type = "z")
z.scores %>% summary()
which( abs(z.scores) >3 )
length (which( abs(z.scores) >3 ))

z.scores <- wilt$GLCM_pan %>%  scores(type = "z")
z.scores %>% summary()
which(abs(z.scores) > 3 )
length(which(abs(z.scores) > 3))

###############################################################################
#Q4.

wilt_sub <- wilt %>% select(Mean_Green, Mean_Red, Mean_NIR, GLCM_pan)
wilt_cap <- as.data.frame(sapply(wilt_sub, FUN = cap))
summary(wilt_cap)
summary(wilt_sub)



###############################################################################
#worksheet questions --- ozone data set
###############################################################################
ozone<- read_csv("C:/Users/ericn/OneDrive/Documents/ozone.csv")

#kable(head(ozone[,1:8]))
#kable(head(ozone[,9:13]))

###############################################################################
#Q5.

boxplot(ozone$ozone_reading)
boxplot(ozone$ozone_reading ~ ozone$Month)

boxplot(ozone$ozone_reading)
boxplot(ozone$ozone_reading ~ ozone$Month )
boxplot(ozone$ozone_reading ~ ozone$Wind_speed )

plot(ozone$ozone_reading)
plot(ozone$ozone_reading ~ ozone$Month )
plot(ozone$ozone_reading ~ ozone$Wind_speed )

ozone_sub <- ozone %>% select(ozone_reading, Month, Wind_speed) 
ozone_sub<-ozone_sub[complete.cases(ozone_sub),]
head(ozone_sub)

###############################################################################
#Q6.

#first approach
results <- mvn(data = ozone_sub, multivariateOutlierMethod = "quan", showOutliers = TRUE)
results$multivariateOutliers
ozone_clean <- ozone_sub[ -c(1:35), ]
dim(ozone_clean)

#second approach
ozone_clean2 <- mvn(data = ozone_sub, multivariateOutlierMethod = "quan", showOutliers = TRUE, showNewData = TRUE)
dim(ozone_clean2$newData)
head(ozone_clean2)

summary(ozone_clean2)

###############################################################################
#Q7.

ozone_sub2 <- ozone %>% select(ozone_reading,Temperature_Sandburg)


# Using cut()
ozone_sub2 <- ozone_sub2 %>% 
  mutate(temp = cut(Temperature_Sandburg, 
                    breaks = c(-Inf, 30, 40, 50, 60, 70, 80, 90, Inf)))

# Using cut() with sequence(): min and max of temperature are 20 and 100 

subset_cut<-  cut(ozone_sub2$Temperature_Sandburg ,seq(20,100,10))
subset<- ozone_sub2 %>% mutate(Temperature_band = subset_cut)
head(subset)

# Using case_when()
ozone_sub4 <- ozone_sub2 %>%
  mutate(temp = case_when(Temperature_Sandburg <= 30 ~ "(-Inf, 30]", 
                          Temperature_Sandburg > 30 & Temperature_Sandburg <= 40  ~ "(30, 40]",
                          Temperature_Sandburg > 40 & Temperature_Sandburg <= 50  ~ "(40, 50]",
                          Temperature_Sandburg > 50 & Temperature_Sandburg <= 60  ~ "(50, 60]",
                          Temperature_Sandburg > 60 & Temperature_Sandburg <= 70  ~ "(60, 70]",
                          Temperature_Sandburg > 70 & Temperature_Sandburg <= 80  ~ "(70, 80]",
                          Temperature_Sandburg > 80 & Temperature_Sandburg <= 90  ~ "(80, 90]", 
                          Temperature_Sandburg > 90 ~ "(90, Inf]"))

# Using ifelse()
ozone_sub5 <- ozone_sub2 %>%
  mutate(temp = ifelse(Temperature_Sandburg <= 30, "(-Inf, 30]", 
                       ifelse(Temperature_Sandburg > 30 & Temperature_Sandburg <= 40  , "(30, 40]",
                              ifelse(Temperature_Sandburg > 40 & Temperature_Sandburg <= 50  , "(40, 50]",
                                     ifelse(Temperature_Sandburg > 50 & Temperature_Sandburg <= 60  , "(50, 60]",
                                            ifelse(Temperature_Sandburg > 60 & Temperature_Sandburg <= 70  , "(60, 70]",
                                                   ifelse(Temperature_Sandburg > 70 & Temperature_Sandburg <= 80  , "(70, 80]",
                                                          ifelse(Temperature_Sandburg > 80 & Temperature_Sandburg <= 90  , "(80, 90]", 
                                                                 ifelse(Temperature_Sandburg > 90 , "(90, Inf]", "MISS")))))))))


boxplot(ozone_sub2$ozone_reading ~ ozone_sub2$temp)

kable(head(ozone_sub2))


###############################################################################
#Q8.

ozone_sub <- ozone %>% select(ozone_reading, Month, Wind_speed) 
summary(ozone_sub)
ozone_cap <- as.data.frame(sapply(ozone_sub, FUN = cap))
summary(ozone_cap)
dim(ozone_cap)


# 
ozone_sub <- ozone[,c(1,4,6)]
ozonesub_clean2 <- mvn(data=ozone_sub,multivariateOutlierMethod = "quan",
                       showOutliers = TRUE,showNewData = TRUE)

summary(ozonesub_clean2$newData)

ozone_capped <- sapply(ozone_sub,cap)

################################################################################
#To check normal distribution

#Density plot: provides visual judgment whether distribution is bell shaped.
#QQ plot: draws correlation between a given sample and normal distribution.
#A 45-degree reference line is also plotted. 
#In a QQ plot, each observation is plotted as a single dot. 
#If the data are normal, the dots should form a straight line.
#sample below has normal dist
################################################################################
library("ggpubr")
library(rstatix)

data(ToothGrowth)
ToothGrowth %>% sample_n_by(supp, dose, size = 1)

# Density plot
ggdensity(ToothGrowth$len, fill = "lightgray")
# QQ plot
ggqqplot(ToothGrowth$len)



####################################################


iris <- read_csv("C:/Sona/RMIT/MATH2349_2050/DataWrangling-sem2-2020/DataRepository/iris.csv")
versicolor <- iris %>%  filter( Species == "versicolor" ) %>%  dplyr::select(Sepal.Length, Sepal.Width,Petal.Length )

View(versicolor)
head(versicolor)

results <- mvn(data = versicolor, multivariateOutlierMethod = "quan", showOutliers = TRUE)

results$multivariateOutliers


##########################
ozone<- read_csv("C:/Sona/RMIT/MATH2349_2050/DataWrangling-sem2-2020/DataRepository/ozone.csv")

impute_outliers <- function(x,removeNA = TRUE){
  quantiles <- quantile( x, c(.05, .95 ),na.rm = removeNA )
  x[ x < quantiles[1] ] <- mean(x,na.rm = removeNA )
  x[ x > quantiles[2] ] <- median(x,na.rm = removeNA )
  x
}

imputed_data <- impute_outliers(ozone$pressure_height)

par(mfrow = c(1, 2))

boxplot(ozoneData$pressure_height, main="Pressure Height having Outliers", boxwex=0.3)

boxplot(imputed_data, main="Pressure Height with imputed data", boxwex=0.3)





