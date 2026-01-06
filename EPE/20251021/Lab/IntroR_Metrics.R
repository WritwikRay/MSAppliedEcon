##################################################
## IntroR_Metrics.R
## by Cristian Huse -- cristian.huse@uol.de
## Current version: 20210930
## First version:   20210101
## This code provides an introduction to R focusing 
## on applied econometrics
##################################################



###################################################
# Setup
###################################################

## Clean any objects in workspaces
rm(list=objects())

# load libraries here


## Which folder I'm working from (wd=working directory)
getwd()

# Change folder -- note use of forward bars (/)
setwd("CXXX")

# Check contents of workspace
#ls()
objects()

## Load and install today's packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(mfx, tidyverse, hrbrthemes, estimatr, 
               ivreg, fixest, sandwich, lmtest, 
               margins, vtable, broom, modelsummary,
               data.table, readxl, xlsx, AER, tidyverse, 
               htmltools,lubridate)
## Make sure we have at least version 0.6.0 of ivreg
if (numeric_version(packageVersion("ivreg")) < 
    numeric_version("0.6.0")) 
  install.packages("ivreg")



## Optional -- ggplot2 plotting theme
#theme_set(hrbrthemes::theme_ipsum())



###################################################
# Load data
###################################################

# Define data folder
# "<-" is the assignment operator
dataDir <- paste0(getwd(), "/Data/")

# If data in .csv format:
## One way
df <- read.csv(paste0(dataDir, "data_fuel_ep.csv"),
               header = T,
               row.names = NULL,
               stringsAsFactors = F,
               check.names = F)
?read.csv

# Define as data.frame
df <- as.data.frame(df)
# Drop NAs, if any
df <- na.omit(df)

## Alternatives 
# Using the data.table package
#df <- fread(paste0(dataDir, "data_fuel_ep.csv"), header = T)
# If in .xls or xlsx format:
## One way -- using the readxl package
#df <- read_excel(paste0(dataDir, "data_fuel_ep.xls")
## Another way -- using the xlsx package
#df <- read.xlsx(paste0(dataDir, "data_fuel_ep.xls"),1)
# For Stata .dta files, use library(haven)
#df <- read_dta(fuel)


# Have a look at the data
View(df)

#Check what is in the workspace
ls()
#oops...
ls(df)



#####################################################
## Create basic variables
#####################################################

# Create date variable
# This requires library(lubridate)
df$date<-paste0(df$year,df$month)
df$date<-ym(df$date)
#YYYYMM

# Adjust price for the lower energy content of ethanol w.r.t. gasoline
# Diferences due to E85 in "Summer" and E75 in "Winter"
?ifelse
df$efactor <- ifelse(df$month>=5&df$month<=10, .704698, .7438478)
df$p_e_real_adj <- (df$p_e_real/df$efactor)

ls(df)



#####################################################
## Plots -- using Base R as much as possible
#####################################################

#1a
plot(df$date, df$p_g_real, ylim=c(10,17), type="l", lty=1, col="blue", 
     ylab = "Energy-adj. Prices", xlab = "Date", main = "Fuel Price Dynamics")
lines(df$date, df$p_e_real_adj, lty=2, col="red" )
legend("bottomright", legend = c("Petrol (SEK/liter)","Ethanol (SEK/liter, energy-adj.)"),
       lty=c(1,2),col=c("blue","red"))
# Note that I'm aiming setting ylim() in a way to avoid the data is covered by the legend

#1b -- note difference w.r.t. version in paper
df$petrol.premium <- df$p_g_real-df$p_e_real_adj
df$pe_ratio <- df$p_g_real/df$p_e_real_adj

plot(df$date, df$petrol.premium, ylim=c(-5,2.5), type="l", lty=1, col="blue", 
     ylab = "Petrol Price Premium, P/E Price Ratio", xlab = "Date", main = "Relative Fuel Prices")
lines(df$date, df$pe_ratio, lty=2, col="red" )
legend("bottomleft", legend = c("Petrol Price Premium (SEK/liter)","Petrol-Ethanol Price Ratio"),
       lty=c(1,2),col=c("blue","red"))

#1c -- note difference w.r.t. version in paper
summary(cbind(df$q_e85,df$petrol.premium))
# scale to make series more easily comparable
df$q_e_scaled <- df$q_e85/10000

plot(df$date, df$petrol.premium, ylim=c(-5.5,3), type="l", lty=1, col="blue", 
     ylab = "Petrol Price Premium, Ethanol Sales", xlab = "Date", main = "Relative Fuel Prices vs. Ethanol Sales")
lines(df$date, df$q_e_scaled, lty=2, col="red" )
legend("bottomleft", legend = c("Petrol Price Premium (SEK/liter)","Ethanol Sales (x10^7 liters)"),
       lty=c(1,2),col=c("blue","red"))

#1d -- note difference w.r.t. version in paper
summary(cbind(df$q_e85,df$fleet_ffv))
df$fleet_ffv_scaled<-df$fleet_ffv/10

plot(df$date, df$fleet_ffv_scaled, ylim=c(0,36000), type="l", lty=1, col="blue", 
     ylab = "FFV Fleet, Ethanol Sales", xlab = "Date", main = "FFV Fleet and Ethanol Sales")
lines(df$date, df$q_e85, lty=2, col="red" )
legend("topright", legend = c("FFV Fleet (x10 units)","Ethanol Sales (x10^3 liters)"),
       lty=c(1,2),col=c("blue","red"))

#1e -- note difference w.r.t. version in paper
#Note that here it's the ethanol/gasoline price ratio
df$ep_ratio <- df$p_e_real_adj/df$p_g_real

hist(df$ep_ratio,breaks=12,freq=F, xlab = "Ethanol-Petrol Price Ratio (Energy-adj.)",
     main = "Histogram of Relative Fuel Prices")
lines(density(df$ep_ratio,bw=0.025, lty=1, kernel=c("epa")),col="red")
legend("topright", legend = c("Histogram","Kernel Density (b=0.025)"),
       lty=c(1,1),col=c("gray","red"))


#1f -- note difference w.r.t. version in paper
# Don't worry about what "loess" means at this point
df$e_share <- (df$q_e85*df$efactor)/((df$q_e85*df$efactor)+df$q_g)
# sorting data according to X below
df <- df[order(df$e_share),]

llr <- loess(df$ep_ratio ~ df$e_share, span=0.25, degree=1)
names(llr)
fitted <- predict(llr)

plot(df$e_share,df$ep_ratio, xlim=c(0,0.08), ylim=c(.8,1.3),col="blue",
     main = "Empirical Demand Curve", xlab = "Ethanol Share", 
     ylab = "Ethanol-Petrol Price Ratio (energy-adj.)")

lines(df$e_share, fitted, col="red")

legend("topright", legend = c("Data","Nonparametric Estimate"),
       lty=c(1,1),col=c("blue","red"))
# Note difference in fit: above is nonparametric whereas in paper it is a
# semiparametric (Robinson's partially linear regression estimator, see paper)
# e.g., one will control for ffv_fleet, month and year fixed-effects

# Order the data in time as before
df <- df[order(df$time),]



####################################################################
## What if you were to generate a pdf files combining all plots?
####################################################################

# Destination folder + file
fig_folder = 'C:\\Users\\huse-admin\\Dropbox\\CRISTIAN\\Teaching\\Cursos_Meus\\Teaching_2022\\EPE\\Lab3_RMetrics\\Figs'

# Open pdf graphics device
# equivalent for other formats, e.g., postscript, svg, png, jpeg, bmp...
#?pdf
pdf(file=paste0(fig_folder,'\\my_plot.pdf'),paper = "a4r",
    width = .9*12, height = .9*9, bg = "white", colormodel = "srgb",)
# width, height are measured in inches

# To the plots
# Output in a 3x2 matrix (this needs to be done after calling pdf)
par(mfrow=c(3,2))
#1a
df <- df[order(df$time),]
plot(df$date, df$p_g_real, ylim=c(10,17), type="l", lty=1, col="blue", 
     ylab = "Energy-adj. Prices", xlab = "Date", main = "Fuel Price Dynamics")
lines(df$date, df$p_e_real_adj, lty=2, col="red" )
legend("bottomright", legend = c("Petrol (SEK/liter)","Ethanol (SEK/liter, energy-adj.)"),
       lty=c(1,2),col=c("blue","red"))
# Note that I'm aiming setting ylim() in a way to avoid the data is covered by the legend

#1b -- note difference w.r.t. version in paper
df$petrol.premium <- df$p_g_real-df$p_e_real_adj
df$pe_ratio <- df$p_g_real/df$p_e_real_adj

plot(df$date, df$petrol.premium, ylim=c(-4,2.5), type="l", lty=1, col="blue", 
     ylab = "Petrol Price Premium, P/E Price Ratio", xlab = "Date", main = "Relative Fuel Prices")
lines(df$date, df$pe_ratio, lty=2, col="red" )
legend("bottomleft", legend = c("Petrol Price Premium (SEK/liter)","Petrol-Ethanol Price Ratio"),
       lty=c(1,2),col=c("blue","red"))

#1c -- note difference w.r.t. version in paper
summary(cbind(df$q_e85,df$petrol.premium))
# scale to make series more easily comparable
df$q_e_scaled <- df$q_e85/10000

plot(df$date, df$petrol.premium, ylim=c(-5.5,3), type="l", lty=1, col="blue", 
     ylab = "Petrol Price Premium, Ethanol Sales", xlab = "Date", main = "Relative Fuel Prices vs. Ethanol Sales")
lines(df$date, df$q_e_scaled, lty=2, col="red" )
legend("bottomleft", legend = c("Petrol Price Premium (SEK/liter)","Ethanol Sales (x10^7 liters)"),
       lty=c(1,2),col=c("blue","red"))

#1d -- note difference w.r.t. version in paper
summary(cbind(df$q_e85,df$fleet_ffv))
df$fleet_ffv_scaled<-df$fleet_ffv/10

plot(df$date, df$fleet_ffv_scaled, ylim=c(0,36000), type="l", lty=1, col="blue", 
     ylab = "FFV Fleet, Ethanol Sales", xlab = "Date", main = "FFV Fleet and Ethanol Sales")
lines(df$date, df$q_e85, lty=2, col="red" )
legend("topright", legend = c("FFV Fleet (x10 units)","Ethanol Sales (x10^3 liters)"),
       lty=c(1,2),col=c("blue","red"))

#1e -- note difference w.r.t. version in paper
#Note that here it's the ethanol/gasoline price ratio
df$ep_ratio <- df$p_e_real_adj/df$p_g_real

hist(df$ep_ratio,breaks=12,freq=F, xlab = "Ethanol-Petrol Price Ratio (Energy-adj.)",
     main = "Histogram of Relative Fuel Prices")
lines(density(df$ep_ratio,bw=0.025, lty=1, kernel=c("epa")),col="red")
legend("topright", legend = c("Histogram","Kernel Density (b=0.025)"),
       lty=c(1,1),col=c("gray","red"))

#1f -- note difference w.r.t. version in paper
df$e_share <- (df$q_e85*df$efactor)/((df$q_e85*df$efactor)+df$q_g)
# sorting data according to X below
df <- df[order(df$e_share),]

llr <- loess(df$ep_ratio ~ df$e_share, span=0.25, degree=1)
names(llr)
fitted <- predict(llr)

plot(df$e_share,df$ep_ratio, xlim=c(0,0.08), ylim=c(.8,1.3),col="blue",
     main = "Empirical Demand Curve", xlab = "Ethanol Share", 
     ylab = "Ethanol-Petrol Price Ratio (energy-adj.)")

lines(df$e_share, fitted, col="red")

legend("topright", legend = c("Data","Nonparametric Estimate"),
       lty=c(1,1),col=c("blue","red"))
# Note difference in fit: above is nonparametric whereas in paper it is a
# semiparametric (Robinson's partially linear regression estimator, see paper)
# e.g., one will control for ffv_fleet, month and year fixed-effects

#Back to default
par(mfrow=c(1,1))
# Turn off the plot device
dev.off()

# The plot will be generated in the folder and not rendered on the right
# Note: The original plots don't have titles due to rules of the journal



#####################################################
## Exploratory data analysis, Descriptive statistics
#####################################################

#####################################################
## Tables with descriptive statistics
#####################################################
# This requires library(modelsummary)
# see https://vincentarelbundock.github.io/modelsummary/articles/datasummary.html

# Table with summary statistics (better than summary(df))
datasummary_skim(df)
# Note that R isn't as good with labelling variables as other software

# A more concise version
datasummary(All(df) ~ N + Mean + SD + Min + Max, data = df)
# With title and in latex format (check your wd)
#datasummary(All(df) ~ N + Mean + SD + Min + Max, data = df, title="Descriptive Statistics for the Swedish Fuel Market",output='table.tex')

# Correlation matrix
datasummary_correlation(df)

# (Contemporary) Correlation matrix of subset of variables
## Price of gasoline in SE vs. international oil prices
tmp<-df[,c("p_g_real","POILBRE","POILWTI")]
datasummary_correlation(tmp)
## Price of sugar in SE vs. commodities
tmp<-df[,c("p_e_real","PFOOD","PMAIZMT","PSUGAEEC")]
datasummary_correlation(tmp)
# Why would I be interested in these correlations?

# Removing tmp variables
ls()
rm(tmp)





#####################################################
## Econometrics -- Regression analysis
#####################################################



#####################################################
# Data transformations: logs and lags
#####################################################

#First, define logged versions of key variables (log(.) returns natural log)
#Note they start with df$
df$lqe<-log(df$q_e85)
df$lqg<-log(df$q_g)
#df$lpe_unadj<-log(df$p_e_real)
df$lpg<-log(df$p_g_real)
df$lpe <- log(df$p_e_real/df$efactor)

# There are different ways to lag variables, from base to dplyr::mutate()

# Order the data in terms of time
df <- df[order(df$time),]

# PFOOD, PMAIZMT,POILBRE,POILWTI,PSUGAEEC
# L1 for oil prices --> gasoline prices
df$LPOILBRE_L1 <- log(lag(df$sek_usd_last*df$POILBRE,1))
df$LPOILWTI_L1 <- log(lag(df$sek_usd_last*df$POILWTI,1))

# L3 for commodity prices --> ethanol prices
df$LPFOOD_L3 <- log(lag(df$sek_usd_last*df$PFOOD,3))
df$LPMAIZMT_L3 <- log(lag(df$sek_usd_last*df$PMAIZMT,3))
df$LPSUGAEEC_L3 <- log(lag(df$sek_usd_last*df$PSUGAEEC,3))

# L3 for fuel prices --> fleet
df$lpg_L3 <- lag(df$lpg,3)
df$lpe_L3 <- lag(df$lpe,3)



# Plotting endogenous variables and potential instruments
par(mfrow=c(1,1))
# Gasoline
summary(df$lpg); summary(df$LPOILBRE_L1); summary(df$LPOILWTI_L1)

plot(df$date, df$lpg, type="l", ylim=c(-1,7), lty=1, col="blue", 
     ylab = "Log gasoline prices", xlab = "Date", main = "Gasoline and Potential Instruments")
lines(df$date, df$LPOILBRE_L1, lty=2, col="red" )
lines(df$date, df$LPOILWTI_L1, lty=3, col="green" )

legend("bottomright", legend = c("Gasoline Price","Brent Oil Price","WTI Oil Price"),
       lty=c(1,2,3),col=c("blue","red","green"))

# Ethanol
summary(df$lpe); summary(df$LPFOOD_L3); summary(df$LPMAIZMT_L3); summary(df$LPSUGAEEC_L3)

plot(df$date, df$lpe, type="l", ylim=c(-3,8), lty=1, col="blue", 
     ylab = "Log ethanol prices", xlab = "Date", main = "Ethanol and Potential Instruments")
lines(df$date, df$LPFOOD_L3, lty=2, col="red" )
lines(df$date, df$LPMAIZMT_L3, lty=3, col="green" )
lines(df$date, df$LPSUGAEEC_L3, lty=3, col="black" )

legend("bottomright", legend = c("Ethanol Price","Food Price Index","Maize Price","Sugar Price"),
       lty=c(1,2,3,4),col=c("blue","red","green","black"))

# Fleet
# Note differences in magnitude
summary(df$fleet_g_ffv); summary(df$fleet_ffv); summary(df$lpg_L3); summary(df$lpe_L3);  

plot(df$date, df$fleet_g_ffv, type="l", lty=1, col="blue", 
     ylab = "Fleets and lagged fuel prices", 
     xlab = "Date", main = "Fleets and Potential Instruments")
lines(df$date, df$fleet_ffv, lty=2, col="red" )
lines(df$date, df$lpg_L3, lty=3, col="green" )
lines(df$date, df$lpe_L3, lty=3, col="black" )

legend("bottomleft", 
       legend = c("G+FFV Fleet","FFV Fleet","Lagged Gasoline Price","Lagged Ethanol Price"),
       lty=c(1,2,3,4),col=c("blue","red","green","black"))




#####################################################
## OLS Estimation
#####################################################

# Recall lm(y ~ x1 + x2 + x3 + ..., data = df), where lm() fits a linear model
ols_g1<-lm(df$lqg ~ df$lpg + df$lpe)
ols_e1<-lm(df$lqe ~ df$lpg + df$lpe)
summary(ols_g1); summary(ols_e1)
# Note the need of further controls

# OLS with fixed-effects -- month, year FEs
# ST 3
# Note for fixest::feols(): drop df$ and add data=df
st3_g <- feols(lqg ~ lpg + lpe + fleet_g_ffv + as.factor(month) + as.factor(year), data=df)
st3_e <- feols(lqe ~ lpg + lpe + fleet_ffv + as.factor(month) + as.factor(year), data=df)
# equivalent
st3_g2 <- feols(lqg ~ lpg + lpe + fleet_g_ffv | month + year, data=df)
st3_e2 <- feols(lqe ~ lpg + lpe + fleet_ffv | month + year, data=df)
summary(st3_g); summary(st3_e)
# Downward-sloping demands -- good start!



#####################################################
## IV estimation -- 2SLS
#####################################################
# Remember that there are different possibilities here:
#1. ivreg::ivreg()
#2. estimatr::iv_robust()
#3. fixest::feols() --> we will focus on this one

# 2SLS with fixed-effects -- month, year FEs
# ST4
# Assuming fleet is exogenous, i.e., only fuel prices are endogenous
st4_g <- feols(lqg ~ fleet_g_ffv | # y ~ ex
                 month + year | # FEs
                 lpg + lpe ~ 
                 LPOILBRE_L1 + LPOILWTI_L1 + 
                 LPFOOD_L3 + LPMAIZMT_L3 + LPSUGAEEC_L3,
               data = df  )

st4_e <- feols(lqe ~ fleet_ffv | # y ~ ex
                 month + year | # FEs
                 lpg + lpe ~ 
                 LPOILBRE_L1 + LPOILWTI_L1 + 
                 LPFOOD_L3 + LPMAIZMT_L3 + LPSUGAEEC_L3,
               data = df)

summary(st4_g); summary(st4_e)

# Assuming fleet is endogenous -- (1)-(2) in ST4
st4_g2 <- feols(lqg ~ as.factor(month) + as.factor(year) | # y ~ ex
                  lpg + lpe +fleet_g_ffv ~  
                  LPOILBRE_L1 + LPOILWTI_L1 + 
                  LPFOOD_L3 + LPMAIZMT_L3 + LPSUGAEEC_L3 +
                  hours_tot + lpg_L3 + lpe_L3,
                data = df)

st4_e2 <- feols(lqe ~ as.factor(month) + as.factor(year) | # y ~ ex
                  lpg + lpe +fleet_ffv ~  
                  LPOILBRE_L1 + LPOILWTI_L1 + 
                  LPFOOD_L3 + LPMAIZMT_L3 + LPSUGAEEC_L3 +
                  hours_tot + lpg_L3 + lpe_L3,
                data = df)

summary(st4_g2); summary(st4_e2)





####################################################
# Reporting results
####################################################

# Check https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html
# for nice examples

# Setting up a nice table with regression estimates
modelsummary(list(st3_g, st3_e, st4_g, st4_e, st4_g2, st4_e2), 
             stars = c("*" = .1, "**" = .05, "***" = .01), fmt = 3, 
             gof_omit = "AIC|BIC|Log.Lik.|R2 Adj.|R2 Within|R2 Pseudo")

# Save as latex file
modelsummary(list(st3_g, st3_e, st4_g, st4_e, st4_g2, st4_e2), 
             stars = c("*" = .1, "**" = .05, "***" = .01), fmt = 3, 
             gof_omit = "AIC|BIC|Log.Lik.|R2 Adj.|R2 Within|R2 Pseudo", 
             output = "my_first_table.tex")

# Omit the intercept
modelsummary(list(st3_g, st3_e, st4_g, st4_e, st4_g2, st4_e2), 
             stars = c("*" = .1, "**" = .05, "***" = .01), fmt = 3, 
             gof_omit = "AIC|BIC|Log.Lik.|R2 Adj.|R2 Within|R2 Pseudo", 
             coef_omit = "Intercept")

# Omit the intercept, report only *.*lpg, *.*lpe terms
modelsummary(list(st3_g, st3_e, st4_g, st4_e, st4_g2, st4_e2), 
             stars = c("*" = .1, "**" = .05, "***" = .01), fmt = 3, 
             gof_omit = "AIC|BIC|Log.Lik.|R2 Adj.|R2 Within|R2 Pseudo", 
             coef_omit = "^(?!.*lpg)(?!.*lpe)(\\(Int)")

# Label coefficients
cm <- c('lpg'    = 'log(Gasoline price)',
        'lpe'    = 'log(Ethanol price)',
        'fit_lpg'    = 'log(Gasoline price)',
        'fit_lpe'    = 'log(Ethanol price)'
)
models <- list(st3_g, st3_e, st4_g, st4_e, st4_g2, st4_e2)
modelsummary(models, coef_map = cm, 
             stars = c("*" = .1, "**" = .05, "***" = .01), fmt = 3, 
             gof_omit = "AIC|BIC|Log.Lik.|R2 Adj.|R2 Within|R2 Pseudo"
)

# Label columns
models <- list(
  "OLS-Gasoline" = st3_g,
  "OLS-Ethanol" = st3_e,
  "2SLS-Gasoline (V1)" = st4_g,
  "2SLS-Ethanol (V1)" = st4_e,
  "2SLS-Gasoline (V2)" = st4_g2,
  "2SLS-Ethanol (V2)" = st4_e2
)

modelsummary(models, coef_map = cm, 
             stars = c("*" = .1, "**" = .05, "***" = .01), fmt = 3, 
             gof_omit = "AIC|BIC|Log.Lik.|R2 Adj.|R2 Within|R2 Pseudo|Std.Errors"
)

# Save as latex file
modelsummary(models, coef_map = cm, 
             stars = c("*" = .1, "**" = .05, "***" = .01), fmt = 3, 
             gof_omit = "AIC|BIC|Log.Lik.|R2 Adj.|R2 Within|R2 Pseudo|Std.Errors",
             output = "my_second_table.tex"
)


