####################################
# HW 13
####################################

# Prepwork 
# --------------------------------------

# Loading libraries 
library(tidyverse) # so we can have dplyr 

# Loading data 
dragon = read.csv("dragon_data.csv")


###################################################
# OBJECTIVE 1: model this with linear regression 
###################################################

# y = mx + b 
plot(acres_on_fire ~ size, data = dragon, main = "Acres burned vs Size of dragon", xlab = "Size of dragon (tons)", ylab = "Acres set on fire")         
# plot our dragon damage
fit = lm(acres_on_fire ~ size, data = dragon)      # fit a linear model 
abline(fit, col = "red")                           # plot our model 
summary(fit)
m1 = 1.34669 
b1 = -1.37555 

# Estimated Parameters: 
# acres burned = 1.35*size of dragon - 1.37  

#######################################################
# OBJECTIVE 2: model this with ordinary least squares
#######################################################

# A : Grid search 
# -----------------------------------------------------

# now imagine that we have this plot and no idea what the fit(lm) is, let's find that fit with ordinary least squares grid search 
  
# I think the intercept looks like it's below 10, so 0:9 would be reasonable here, You can't have a negative dragon size 
# I think the slope looks about 50/40 = 1.25 so a reasonable range would probably be between 1.3 and 1.5 

slopes = seq(from = 1.3, to = 1.5, length.out = 15)
slopes = round(slopes, 2) 
intercepts = seq(from = 0, to = 9, length.out = 15) 
intercepts = round(intercepts, 2) 

# tried making a tibble but didn't work, making matrix instead 
grid = matrix(NA, nrow = 15, ncol = 15)
rownames(grid) = intercepts
colnames(grid) = slopes
grid
as_tibble(grid)   # I don't know why I like tibbles but I do 

# defining things, we will need this for hte loop 
x = dragon$size
y = dragon$acres_on_fire
n = length(y) 

# - log likelihood = - log(likelihood) 
# Likelihood = probability of the outcome 
# ended up looking up equation for negative log likelihood and getitng help with the loop 

# we need to loop over and over to fill up our empty tibble 
for (i in 1:length(intercepts)) {
  for (j in 1:length(slopes)) {
    
    b = intercepts[i]
    m = slopes[j]
    
    # residuals = Observed - predicted 
    residuals = y - (b + m*x)     
    
    # sum of squares 
    rss = sum(residuals^2) 
    
    grid[i, j] = rss # store it in the empty grid 
  }
}

#find the minimum value 

min_val <- min(unlist(grid), na.rm = TRUE)
# this turns the tibble into just a bunch of unorgranized numbers (unlist) and then finds the minimum (min). I will then search that minimum in my table 
min_val
# I used ctrl + f to find the min value in my grid, which turned out to be 1050.125, and it corresponded with intercept = 0 and x = 1.31 

b2a = 0 
m2a = 1.31 

# B. Optim() route 
# --------------------------------------------------

# ok this is like what we did in class 

# sample data 
dat = dragon
# create a little dataframe very linear, we will use our dragon data here 

# Objective function (minimize RSS) 
min_RSS = function(par, data) { 
  with(data,sum((par[1] + par[2]*x-y)^2))
}
# this tells optim what to minimize 
# par = vector of the parameters we want to estimate 
# par[1] = intercept(B0) 
# par[2] = slope(B1) 
# Residuals squared = sum
#   predicted value yhat = par[1] + par[2]*x 
#   residuals = predicted value - y 
#   square of residuals = residuals^2 
#   sum of residuals =  sum(residuals) 
#   All together, Residual Sum of Squares =  sum((par[1] + par[2]*x - y)^2) 

# Initial parameter guess 
initial_params = c(0,1)
# our starting guess is intercept = 0, slope = 1 

# Run optim 
result = optim(par = initial_params, fn = min_RSS, data = dat) 
# par = estimate of intercept and slope, initial guess 
# fn = funciton to minimize RSS 
# dat = dataset 
# value = minimum sum fo squares likelihood 
# counts = how many times it tried different candidate values 
# convergence = will be 0 if it did not converge 
# gradient = how much likelihood is changing when it stopped 

result 
# this list has all our results 
# $par = estimated parameters ~ true answers 
# $value = minimum RSS value 
# $counts = how many times the optimizer evaluated the function 
# $convergence = did it converge? 0 = success, nonzero = did not converge. IE did it settle on a minimum it was happy with? if it did, yo uget convergence. If not, it's mad nad your t hing failed. 
# Alright, our optim route says that the equation is: 
# areas burned = 1.35*dragon_size - 1.376 

m2b = 1.346735 
b2b = -1.376213 

# C. Did it converge? 
#----------------------------------------------------
# yes! 
result$convergence
# is zero. This means the model settled on a minimum it was happy with. 

######################################################
# OBJECTIVE 3: 
######################################################

# NOTES FROM CLASS 

# objective 3 will involve variance, we need to search for it or something 
# variance of the residuals? 
# plug it in from objective 2 
# search for 3 parameters, slope intercept, variance 
# just put in a value for hte variance, variance from residuals from part 2 


# A: Grid search 
# ---------------------------------------------------------
# Uh oh, this math is way beyond me 
# I need to finish the Essington Ch 8 reading 

# first, let's copy and paste from Objective 1 because htis is going to be very similar, but with a different equation 
# I think the intercept looks like it's below 10, so 0:9 would be reasonable here, You can't have a negative dragon size 
# I think the slope looks about 50/40 = 1.25 so a reasonable range would probably be between 1.3 and 1.5 

slopes = seq(from = 1.3, to = 1.5, length.out = 15)
slopes = round(slopes, 2) 
intercepts = seq(from = 0, to = 9, length.out = 15) 
intercepts = round(intercepts, 2) 

# tried making a tibble but didn't work, making matrix instead 
grid = matrix(NA, nrow = 15, ncol = 15)
rownames(grid) = intercepts
colnames(grid) = slopes
grid
as_tibble(grid)   # I don't know why I like tibbles but I do 

# defining things, we will need this for hte loop 
x = dragon$size
y = dragon$acres_on_fire
n = length(y) 

# - log likelihood = - log(likelihood) 
# Likelihood = probability of the outcome 
# ended up looking up equation for negative log likelihood and getitng help with the loop 

# we need to loop over and over to fill up our empty tibble 
for (i in 1:length(intercepts)) {
  for (j in 1:length(slopes)) {
    
    b = intercepts[i]
    m = slopes[j]
    
    # residuals
    residuals = y - (b + m*x)
    
    # sigma^2 MLE
    sigma2 = mean(residuals^2)  # this is hte variance Olaf was talking about!! Variance = avg(residuals^2) 
    
    # negative log likelihood
    nll = (n/2)*log(2*pi*sigma2) + (1/(2*sigma2))*sum(residuals^2)
    
    grid[i, j] = nll
  }
}

#find the minimum value 

min_val <- min(unlist(grid), na.rm = TRUE)
# this turns the tibble into just a bunch of unorgranized numbers (unlist) and then finds the minimum (min). I will then search that minimum in my table 
min_val
# I used ctrl + f to find the min value (147.063) in my grid, it corresponded with intercept = 0 and x = 1.31 too 

m3a = 1.31 
b3a = 0 

# B: Optim search 
# ---------------------------------------------------------

# alright, we need to do the same thing as last time but instead of creating a rss function, we need to create a negative log likelihood function 

# sample data 
dat = dragon

# make a function for log likelihood, it'll be super similar to Olaf's but iwth the extra steps for the negative log likelihood 
min_llh = function(par, data) { 
  # residuals
  residuals = y - (b + m*x)
  
  # sigma^2 MLE
  sigma2 = mean(residuals^2)  # this is hte variance Olaf was talking about!! Variance = avg(residuals^2) 
  
  # negative log likelihood
  nll = (n/2)*log(2*pi*sigma2) + (1/(2*sigma2))*sum(residuals^2)
  
  # now plug it into the formula 
  with(data, nll)
}
# holy smokes did it work first try?! 

# Initial parameter guess 
initial_params = c(0,1)
# our starting guess is intercept = 0, slope = 1 

# Run optim 
result = optim(par = initial_params, fn = min_RSS, data = dat) 
# par = estimate of intercept and slope, initial guess 
# fn = funciton to minimize RSS 
# dat = dataset 
# value = minimum sum fo squares likelihood 
# counts = how many times it tried different candidate values 
# convergence = will be 0 if it did not converge 
# gradient = how much likelihood is changing when it stopped 

# C: Did it converge 
# --------------------------------------
result 
# It did!!! convergence = 0 
# Estimated equation is: acres burned = 1.34*dragon size - 1.376 

m3b = 1.346735 
b3b = -1.376213

#######################################################
# OBJECTIVE 4 
#######################################################

my_tibble = tibble(
  category = c("Linear Regression","LSS Manual","LSS Optim","MLL Manual","MLL Optim"), 
  Beta = round((c(m1, m2a, m2b, m3a, m3b), 4), 
  Intercept = c(b1, b2a, b2b, b3a, b3b)
))

# I do not know why it is rounding itself :( they are actually slightly different but roudned htey turn into the same thing 
# The only difference is that my manual fits have an intercept of 0 because I assumed that you can't have a negatively sized dragon and manually set hte minimum intercept to be at 0 


#####################################################33
# Garbageland
#####################################################

library(tibble) #Tibbles are basically dataframes
my_tibble = tibble(
  x = 1:5,
  y = letters[1:5],
  z = x*2
)

acres_predicted = predict(fit)

# Objective function (minimize RSS) 
min_RSS = function(par, data) { 
  with(data,sum((par[1] + par[2]*x-y)^2))
}

predict_function = function(x) {
  1.35*x - 1.37
}
predict_function(20)

# objective 3 will involve variance, we need to search for it or something 
# variance of the residuals? 
# plug it in from objective 2 
# search for 3 parameters, slope intercept, variance 
# just put in a value for hte variance, variance from residuals from part 2 


# OK I think 
