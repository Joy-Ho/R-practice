library(readr)
library(dplyr)
library(forecast)
library(infotheo)

minmaxnormalise <- function(x){(x- min(x)) /(max(x)-min(x))}

########################################################
## Q1 # Data Transformation
########################################################
candy <- read_csv("candy_production.csv")
head(candy)

candy$production <- candy$IPG3113N
candy <- candy[, -2] # remove IPG3113N attribute
head(candy)

par(mfrow = c(1, 4))

# Use hist() to check the shape of the distribution of production variable in candy data set.
hist(candy$production)

# Apply data transformation via mathematical operations such as 
# log base 10, log base e, square root and reciprocal transformations. 
logcandy <- log10(candy$production)
hist(logcandy)

lncandy <- log(candy$production)
hist(lncandy)

sqrtcandy <- sqrt(candy$production)
hist(sqrtcandy)

recandy <- 1/candy$production
hist(recandy)

# Apply Box - Cox transformation. 
# After you applied transformations, use hist() and check shape of the distribution for each transformation.
boxcandy <- BoxCox(candy$production, lambda = "auto")
boxcandy
hist(boxcandy)

########################################################
## Q2 # Data Normalisation
########################################################

# Apply mean - centering and scale by the standard deviations without centering to the production variable in candy data set. 

# mean centering
center_candy <-
  scale(candy$production, center = TRUE, scale = FALSE)

center_candy
hist(center_candy)

# without centering
scale_candy2 <-
  scale(candy$production,
        center = FALSE,
        scale = sd(candy$production))

hist(scale_candy2)
# Use hist() to check the shape of the distribution for both normalisations you applied.





########################################################
## Q3 # z Score Standardisation and Min- Max Normalisation
########################################################

z_candy <- scale(candy$production, center = TRUE, scale = TRUE)
z_candy
hist(z_candy)


minmaxnormalise <- function(x){
  (x- min(x)) /(max(x)-min(x))
  }

candy_minmax <- minmaxnormalise(candy$production)
candy_minmax
hist(candy_minmax)


########################################################
## Q4 # Binning (a.k.a. Discretisation)
########################################################

# equal width
candy_ew = discretize(candy$production,
                      disc = "equalwidth")
candy_ew %>% head(15)

# equal depth
candy_ed = discretize(candy$production,
                      disc = "equalfreq")
candy_ed %>% head(15)


########################################################
## Q5 #Data Transformation via Mathematical Operations
########################################################
ozone <- read.csv("ozone.csv")

# Subset variables ozone_reading, pressure_height, Pressure_gradient, Visibility, Inversion_temperature 
# from ozone data set and name it ozone_sub.
ozone_sub <-
  ozone %>% dplyr::select(
    ozone_reading,
    pressure_height,
    Pressure_gradient,
    Visibility,
    Inversion_temperature
  )

# Use hist() to check the shape of the distribution for all the variables.
# Apply log base 10, log base e and square root transformations to the variables.
# Check the shape of the distribution of the variables using hist().

par(mfrow = c(1, 5))

# ln
log_ozone_reading <- log(ozone_sub$ozone_reading)
hist(log_ozone_reading)

log_pressure_height <- log(ozone_sub$pressure_height)
hist(log_pressure_height)

log_Pressure_gradient <- log(ozone_sub$Pressure_gradient)
hist(log_Pressure_gradient)

log_Visibility <- log(ozone_sub$Visibility)
hist(log_Visibility)

log_Inversion_temperature <- log(ozone_sub$Inversion_temperature)
hist(log_Inversion_temperature)

# sqrt
sqrt_ozone_reading <- sqrt(ozone_sub$ozone_reading)
hist(sqrt_ozone_reading)

sqrt_pressure_height <- sqrt(ozone_sub$pressure_height)
hist(sqrt_pressure_height)

sqrt_Pressure_gradient <- sqrt(ozone_sub$Pressure_gradient)
hist(sqrt_Pressure_gradient)

sqrt_Visibility <- sqrt(ozone_sub$Visibility)
hist(sqrt_Visibility)

sqrt_Inversion_temperature <- sqrt(ozone_sub$Inversion_temperature)
hist(sqrt_Inversion_temperature)



# #command + shift + C
# par(mfrow = c(1, 5))
# colnames <- colnames(ozone_sub)
# # use unlist to subset data for each column to draw histogram
# for(i in 1:5){
#   hist(unlist(ozone_sub[, i]), main = colnames[i])
# }
# 
# # same as above
# hist(unlist(ozone_sub[, 1]), main = colnames[1])
# hist(unlist(ozone_sub[, 2]), main = colnames[2])
# hist(unlist(ozone_sub[, 3]), main = colnames[3])
# hist(unlist(ozone_sub[, 4]), main = colnames[4])
# hist(unlist(ozone_sub[, 5]), main = colnames[5])
# 
# ozone_log <- sapply(ozone_sub, log10)
# for (i in 1:5){
#   hist(unlist(ozone_log[, i]), main = colnames[i])
# }
# 
# ozone_ln <- sapply(ozone_sub, log)
# for (i in 1:5){
#   hist(unlist(ozone_ln[, i]), main = colnames[i])
# }
# 
# ozone_sqrt <- sapply(ozone_sub, log)
# for (i in 1:5){
#   hist(unlist(ozone_sqrt[, i]), main = colnames[i])
# }


########################################################
## Q6 # Centering and Scaling
########################################################

# Apply mean-centering to ozone_sub data frame using apply() function.
center_ozone <-
  apply(ozone_sub, 2, function(x)
    scale(x, center = TRUE, scale = FALSE))


# Check the shape of the distribution of the variables using hist().

for(i in 1:5){
  hist(unlist(center_ozone[, i]), main = colnames[i])
}


########################################################
## Q7 # Min- Max Normalisation
########################################################

# Use min-max normalisation to the ozone_sub data frame.
minmax_ozone <- sapply(ozone_sub, minmaxnormalise)

ozone_sub2 <- ozone_sub[complete.cases(ozone_sub), ]

minmax_ozone2 <- sapply(ozone_sub2, minmaxnormalise)

for(i in 1:5){
  hist(unlist(minmax_ozone2[, i]), main = colnames[i])
}

# Use hist() to check the shape of the distributions of the variables.

########################################################
## Q8 # Binning
########################################################

# Use ozone_reading variable from ozone dataset 
# and apply equal width (distance) 
# and equal depth (frequency) binning.

ozone_read <- ozone %>% dplyr::select(ozone_reading)

ozone_ew <- discretize(ozone_read, disc = "equalwidth")
ozone_ewbind <- cbind(ozone_read, ozone_ew)
ozone_ewbind %>% head(15)

ozone_ed <- discretize(ozone_read, disc = "equalfreq")
ozone_edbind <- cbind(ozone_read, ozone_ed)
ozone_edbind %>% head(15)

# Compare the variable before and after binning


# use cbind() and show 15 observations from the outputs.

########################################################
## Q9 # Data Challenge
########################################################

# Use ozone_sub data frame and apply Box Cox transformation using apply() function.

# Show the shape of the distibution of the variables using hist()


########################################################
## Q10 # Bonus Exercise
########################################################
