# Import libraries
library(data.table)
library("doMC")
library(future)
library(RCurl)
library(googledrive)
library(ipumsr)
library(stringi)
library(ggplot2)
library(gridExtra)
library(stringr)
library(patchwork)
library(stargazer)
plan(multiprocess)
# disable scientific notation
options(scipen = 999)
registerDoMC(cores = 4)

##############################################################################
#AGGREGATION SECTION
##############################################################################


# Set to my personal directory - adjust accordingly
setwd("~/Google Drive/Non-Academic Work/Research/Zhang/houses/")

ACS_url <- "https://drive.google.com/file/d/1uu-MMSrEFL-QC5TbVXlfUC7OMKMlI9Dj/view?usp=sharing"
fname <- "Datasets/Imported/ACS/usa_00011.dat"
if (!(file.exists(fname))) {
  ret <- drive_download(ACS_url, fname, overwrite = TRUE)
}
# read in ACS data
ddi <- read_ipums_ddi("Datasets/Imported/ACS/usa_00011.xml")
data <- data.table(read_ipums_micro(ddi))

fwrite(data, "ACS.csv")

#creating new variable indicating whether th eindividual owns a home or not
data[, homeownershp :=  ifelse(OWNERSHP == 1, 1, 0)]
#
# HOME OWNERSHIP RATE SECTION
#
#finding average home ownership rate for each age and year
homeownershp_rates <- data[, mean(homeownershp), by = c("YEAR", "AGE")]
setnames(homeownershp_rates, "V1", "home_owner_rate")
#reodering by age and year
homeownershp_rates <- homeownershp_rates[order(YEAR, AGE)]

#ungrouping year variable
homeownershp_rates[, YEAR:= unlist(YEAR)]



##############################################################################
#GRAPHS AND ANALYSIS SECTION
##############################################################################

#plot of home ownership rate by year
homeown_rate <- ggplot(homeownershp_rates, aes(x = AGE, y = home_owner_rate, colour = YEAR, group = YEAR)) +     
  ggtitle("Home ownership rate by year") +
  xlab("Age") +
  ylab("Home ownership rate") +
  geom_line()

homeown_rate

#save home ownership rate by age graph as output
ggsave("plots/homeownership_rate.jpeg", homeown_rate)

#
# HOUSE PRICE SECTION
#
data[, FIPS := paste(str_pad(STATEFIP, 2, pad = "0"), str_pad(COUNTYFIP, 3, pad = "0"), sep = "")]

#finding house price for people aged 25, 30, 35, 40
mean_home <- data[AGE==25, list(mean(homeownershp), sum(PERWT)), by = c("YEAR", "FIPS")]
setnames(mean_home, c("V1", "V2"), c(paste("mean_", 25, sep =""), paste("PERWT", 25, sep = "_")))

for (i in c(30,35,40)) {
  temp <- data[AGE==i, list(mean(homeownershp), sum(PERWT)), by = c("YEAR", "FIPS")]
  setnames(temp, c("V1", "V2"), c(paste("mean_", i, sep =""), paste("PERWT", i, sep = "_")))
  mean_home <- mean_home[temp, on = c("YEAR", "FIPS")]
}

# read in zillow data
zillow_ym <- fread("Datasets/Imported/Zillow/zillow_county_data_yearmonth_clean.csv")
zillow_ym[, fips := str_pad(fips, 5, pad = "0")]
year_med <- zillow_ym[,median(zhvi), by = c("fips", "year")]
setnames(year_med, "V1", "med_price")
year_med <- year_med[year >= 2006]
#merge median zillow house price with county-age gropus
mean_home <- mean_home[year_med, on = c("YEAR" = "year", "FIPS" = "fips")]
mean_home <- na.omit(mean_home)

#filter out outliers
mean_home <- mean_home[med_price < quantile(med_price, .99) & med_price > quantile(med_price, .1)]
  
homeprice_25 <- ggplot(mean_home, aes(x=mean_25, y=med_price)) +
  ggtitle("Scatter Plot for age 25") +
  xlab("Home ownership rate") +
  ylab("Median House Price") + 
  geom_point() + 
  geom_smooth()
homeprice_30 <- ggplot(mean_home, aes(x=mean_30, y=med_price)) +
  ggtitle("Scatter Plot for age 30") +
  xlab("Home ownership rate") +
  ylab("Median House Price") + 
  geom_point() + 
  geom_smooth()
homeprice_35 <- ggplot(mean_home, aes(x=mean_35, y=med_price)) +
  ggtitle("Scatter Plot for age 35") +
  xlab("Home ownership rate") +
  ylab("Median House Price") + 
  geom_point() + 
  geom_smooth()
homeprice_40 <- ggplot(mean_home, aes(x=mean_40, y=med_price)) +
  ggtitle("Scatter Plot for age 40") +
  xlab("Home ownership rate") +
  ylab("Median House Price") + 
  geom_point() + 
  geom_smooth()

#grid layout
homeprice <- (homeprice_25+homeprice_30)/(homeprice_35+homeprice_40)
#save home price graph as output
ggsave("plots/homeprice.jpeg", homeprice)

#running regressions on %home ownership against median house price
lm_mp_25 <- lm(mean_25*100 ~ med_price, weights = PERWT_25, data = mean_home)
lm_mp_30 <- lm(mean_30*100 ~ med_price, weights = PERWT_30, data = mean_home)
lm_mp_35 <- lm(mean_35*100 ~ med_price, weights = PERWT_35, data = mean_home)
lm_mp_40 <- lm(mean_40*100 ~ med_price, weights = PERWT_40, data = mean_home)

#exporting table
price_own <- stargazer(lm_mp_25, lm_mp_30, lm_mp_35, lm_mp_40, 
                       title="Regression of Median House Price on Home Ownership", 
                       dep.var.caption = c("Home Ownership Rate (%)"),
                       covariate.labels = c("Median House Price", "Constant"),
                       dep.var.labels = c("Age 25", "Age 30", "Age 35", "Age 40"),
                       align=TRUE, type = "text", out = "tables/price_ownership.html")

#
# INCOME SECTION
#

makeHomeownIncome <- function(x) {
  homeown_inc <- data[AGE==x, list(mean(homeownershp), median(HHINCOME)), by = c("YEAR", "FIPS")]
  setnames(homeown_inc, c("V1", "V2"), c("mean_ownership","med_income"))
  homeown_inc <- homeown_inc[med_income < quantile(med_income, .99) & med_income > quantile(med_income, .1)]
  ggplot(homeown_inc, aes(x=mean_ownership, y=med_income)) +
    ggtitle(paste("Scatter Plot for age", x)) +
    xlab("Home ownership rate") +
    ylab("Median Household Income") + 
    geom_point() +
    geom_smooth()
}
#scatter plots
ownnic_25 <- makeHomeownIncome(25)
ownnic_30 <- makeHomeownIncome(30)
ownnic_35 <- makeHomeownIncome(35)
ownnic_40 <- makeHomeownIncome(40)
#grid
hhincome <- (ownnic_25+ownnic_30)/(ownnic_35+ownnic_40)
#save home price graph as output
ggsave("plots/hhincome.jpeg", hhincome)

homeown_inc <- data[AGE==25, median(HHINCOME), by = c("YEAR", "FIPS")]
setnames(homeown_inc, "V1", "med_income_25")
mean_home_inc <- na.omit(mean_home[homeown_inc, on = c("YEAR", "FIPS")])

#adding median income for each age in county-year
for (x in c(30, 35, 40)) {
  homeown_inc <- data[AGE==x, median(HHINCOME), by = c("YEAR", "FIPS")]
  setnames(homeown_inc, "V1", paste("med_income", x, sep = "_"))
  mean_home_inc <- na.omit(mean_home_inc[homeown_inc, on = c("YEAR", "FIPS")])
}

#adding log income to data
mean_home_inc[,c("med_income_25_log", "med_income_30_log", "med_income_35_log", "med_income_40_log") :=
                lapply(list(med_income_25, med_income_30, med_income_35, med_income_40), log)]

med_income <- mean_home_inc$med_income_25_log
lm_hhi_25 <- lm(mean_25*100 ~ med_income, weights = PERWT_25, data = mean_home_inc)
med_income <- mean_home_inc$med_income_30_log
lm_hhi_30 <- lm(mean_30*100 ~ med_income, weights = PERWT_30, data = mean_home_inc)
med_income <- mean_home_inc$med_income_35_log
lm_hhi_35 <- lm(mean_35*100 ~ med_income, weights = PERWT_35, data = mean_home_inc)
med_income <- mean_home_inc$med_income_40_log
lm_hhi_40 <- lm(mean_40*100 ~ med_income, weights = PERWT_40, data = mean_home_inc)

#exporting regression table
hhincome <- stargazer(lm_hhi_25, lm_hhi_30, lm_hhi_35, lm_hhi_40, 
                      title="Regression of Log Median Household Income on Home Ownership", 
                      dep.var.caption = c("Home Ownership Rate (%)"),
                      dep.var.labels = c("Age 25", "Age 30", "Age 35", "Age 40"),
                      covariate.labels = c("Log Median Income", "Constant"),
                      align=TRUE, type = "text", out = "tables/hhincome_ownership.html")

#
# PRICE-INCOME RATIO SECTION
#

#scatter plot table
homeprice_25 <- ggplot(mean_home_inc, aes(x=mean_25, y=med_price/med_income_25)) +
  ggtitle("Scatter Plot for age 25") +
  xlab("Home ownership rate") +
  ylab("House Price/Income Ratio") + 
  geom_point() + 
  geom_smooth()
homeprice_30 <- ggplot(mean_home_inc, aes(x=mean_30, y=med_price/med_income_30)) +
  ggtitle("Scatter Plot for age 30") +
  xlab("Home ownership rate") +
  ylab("House Price/Income Ratio") + 
  geom_point() + 
  geom_smooth() 
homeprice_35 <- ggplot(mean_home_inc, aes(x=mean_35, y=med_price/med_income_35)) +
  ggtitle("Scatter Plot for age 35") +
  xlab("Home ownership rate") +
  ylab("House Price/Income Ratio") + 
  geom_point() + 
  geom_smooth()
homeprice_40 <- ggplot(mean_home_inc, aes(x=mean_40, y=med_price/med_income_40)) +
  ggtitle("Scatter Plot for age 40") +
  xlab("Home ownership rate") +
  ylab("House Price/Income Ratio") + 
  geom_point() + 
  geom_smooth()

incratio <- (homeprice_25+homeprice_30)/(homeprice_35+homeprice_40)
#save home price ratio graph as output
ggsave("plots/incratio.jpeg", incratio)

#adding log ratio to data
mean_home_inc[,c("med_income_ratio_25_log", "med_income_ratio_30_log", "med_income_ratio_35_log", 
                 "med_income_ratio_40_log") :=
                lapply(list(med_price/med_income_25, med_price/med_income_30, 
                            med_price/med_income_35, med_price/med_income_40), log)]


#income ratio regressions
income_ratio <- mean_home_inc$med_income_ratio_25_log
lm_ratio_25 <- lm(mean_25*100 ~ med_income, weights = PERWT_25, data = mean_home_inc)
income_ratio <- mean_home_inc$med_income_ratio_30_log
lm_ratio_30 <- lm(mean_30*100 ~ med_income, weights = PERWT_30, data = mean_home_inc)
income_ratio <- mean_home_inc$med_income_ratio_35_log
lm_ratio_35 <- lm(mean_35*100 ~ med_income, weights = PERWT_35, data = mean_home_inc)
income_ratio <- mean_home_inc$med_income_ratio_40_log
lm_ratio_40 <- lm(mean_40*100 ~ med_income, weights = PERWT_40, data = mean_home_inc)

#regression table
ownership_incratio <- stargazer(lm_ratio_25, lm_ratio_30, lm_ratio_35, lm_ratio_40, 
                                title="Regression of Log House Price to Income Ratio on Home Ownership", 
                                dep.var.caption = c("Home Ownership Rate (%)"),
                                dep.var.labels = c("Age 25", "Age 30", "Age 35", "Age 40"),
                                covariate.labels = c("Log House Price to Income Ratio", "Constant"),
                                align=TRUE, type = "text", out = "tables/ownership_priceinc_ratio.html")

#
# HOUSE PRICE/INCOME RATIO AND INCOME 
#

#regression on house price/income ratio and income
income_ratio <- mean_home_inc$med_income_ratio_25_log
income <- mean_home_inc$med_income_25_log
lm_ratio_income_25 <- lm(mean_25*100 ~ income_ratio + income, weights = PERWT_25, data = mean_home_inc)
income_ratio <- mean_home_inc$med_income_ratio_30_log
income <- mean_home_inc$med_income_30_log
lm_ratio_income_30 <- lm(mean_30*100 ~ income_ratio + income, weights = PERWT_30, data = mean_home_inc)
income_ratio <- mean_home_inc$med_income_ratio_35_log
income <- mean_home_inc$med_income_35_log
lm_ratio_income_35 <- lm(mean_35*100 ~ income_ratio + income, weights = PERWT_35, data = mean_home_inc)
income_ratio <- mean_home_inc$med_income_ratio_40_log
income <- mean_home_inc$med_income_40_log
lm_ratio_income_40 <- lm(mean_40*100 ~ income_ratio + income, weights = PERWT_40, data = mean_home_inc)

#exporting regression table
ownership_incratio_inc <- stargazer(lm_ratio_income_25, lm_ratio_income_30, lm_ratio_income_35, lm_ratio_income_40, 
                                    title="Regression of Log Ratio and Log Income on Home Ownership", 
                                    dep.var.caption = c("Home Ownership Rate (%)"),
                                    dep.var.labels = c("Age 25", "Age 30", "Age 35", "Age 40"),
                                    align=TRUE, type = "text", out = "tables/ownership_ratio_inc.html")

#
# REGRESSION WITH EXTRA VARIABLES
#

# list of variables
# % white (RACE is 1 for white people)
# % employed (EMPSTAT employed is 1, unemployed is 2)
# % married (1/2 == married, MARST)

# codes for whether someone is white (1 for yes, 0 for no)
data[,white := ifelse(RACE==1, 1, 0)]
#codes for whether someone is employed (1 for yes, 0 for no, -1 for not in labor force)
data[,employed := ifelse(EMPSTAT==1, 1, ifelse(EMPSTAT==2, 0, -1))]
#codes for whether someone is married (1 for yes, 0 for no)
data[,married := ifelse(MARST==1 | MARST == 2, 1, 0)]

# %white, %married, %employed by year-county
race_marriage <- data[, list(mean(white), mean(married)), by = c("YEAR", "FIPS")]
setnames(race_marriage, c("V1", "V2"), c("white", "married"))
#changing to %
employed <- data[employed != -1, mean(employed), by = c("YEAR", "FIPS")]
setnames(employed, c("V1"), c("employed"))

mean_home_inc_add <- mean_home_inc[employed, on = c("YEAR", "FIPS")]
mean_home_inc_add <- mean_home_inc_add[race_marriage, on = c("YEAR", "FIPS")]
mean_home_inc_add <- na.omit(mean_home_inc_add)

#regression on house price/income ratio and income
income_ratio <- mean_home_inc_add$med_income_ratio_25_log
income <- mean_home_inc_add$med_income_25_log
lm_many_25 <- lm(mean_25*100 ~ income_ratio + income + white + employed + married, weights = PERWT_25, data = mean_home_inc_add)
income_ratio <- mean_home_inc_add$med_income_ratio_30_log
income <- mean_home_inc_add$med_income_30_log
lm_many_30 <- lm(mean_30*100 ~ income_ratio + income + white + employed + married, weights = PERWT_30, data = mean_home_inc_add)
income_ratio <- mean_home_inc_add$med_income_ratio_35_log
income <- mean_home_inc_add$med_income_35_log
lm_many_35 <- lm(mean_35*100 ~ income_ratio + income + white + employed + married, weights = PERWT_35, data = mean_home_inc_add)
income_ratio <- mean_home_inc_add$med_income_ratio_40_log
income <- mean_home_inc_add$med_income_40_log
lm_many_40 <- lm(mean_40*100 ~ income_ratio + income + white + employed + married, weights = PERWT_40, data = mean_home_inc_add)

#exporting regression table
ownership_incratio_inc <- stargazer(lm_many_25, lm_many_30, lm_many_35, lm_many_40, 
                                    title="Regression of Many Variables on Home Ownership", 
                                    dep.var.caption = c("Home Ownership Rate (%)"),
                                    dep.var.labels = c("Age 25", "Age 30", "Age 35", "Age 40"),
                                    covariate.labels = c("log house price/income ratio", "log income", "prop. white", "prop. employed",
                                                         "prop. married", "Constant"),
                                    align=TRUE, type = "text", out = "tables/ownership_many.html")


#normalizing variables
mean_home_inc_add[, c("med_price_norm", "med_income_ratio_25_log_norm", "med_income_ratio_30_log_norm", 
                      "med_income_ratio_35_log_norm", "med_income_ratio_40_log_norm", "med_income_25_log_norm", 
                      "med_income_30_log_norm", "med_income_35_log_norm", "med_income_40_log_norm",
                      "income_norm", "white_norm", "employed_norm", "married_norm") := 
                    lapply(list(med_price, med_income_ratio_25_log, med_income_ratio_30_log, 
                                med_income_ratio_35_log, med_income_ratio_40_log, med_income_25_log, 
                                med_income_30_log, med_income_35_log, med_income_40_log, income, 
                                white, employed, married), scale)]


#regression on house price/income ratio and income for normalized vars
income_ratio <- mean_home_inc_add$med_income_ratio_25_log_norm
income <- mean_home_inc_add$med_income_25_log_norm
lm_many_25_norm <- lm(mean_25*100 ~ income_ratio + income + white_norm + employed_norm + married_norm, weights = PERWT_25, data = mean_home_inc_add)
income_ratio <- mean_home_inc_add$med_income_ratio_30_log_norm
income <- mean_home_inc_add$med_income_30_log_norm
lm_many_30_norm <- lm(mean_30*100 ~ income_ratio + income + white_norm + employed_norm + married_norm, weights = PERWT_30, data = mean_home_inc_add)
income_ratio <- mean_home_inc_add$med_income_ratio_35_log_norm
income <- mean_home_inc_add$med_income_35_log_norm
lm_many_35_norm <- lm(mean_35*100 ~ income_ratio + income + white_norm + employed_norm + married_norm, weights = PERWT_35, data = mean_home_inc_add)
income_ratio <- mean_home_inc_add$med_income_ratio_40_log_norm
income <- mean_home_inc_add$med_income_40_log_norm
lm_many_40_norm <- lm(mean_40*100 ~ income_ratio + income + white_norm + employed_norm + married_norm, weights = PERWT_40, data = mean_home_inc_add)

#exporting regression table
ownership_incratio_inc <- stargazer(lm_many_25_norm, lm_many_30_norm, lm_many_35_norm, lm_many_40_norm, 
                                    title="Normalized Regression of Many Variables on Home Ownership", 
                                    dep.var.caption = c("Home Ownership Rate (%)"),
                                    dep.var.labels = c("Age 25", "Age 30", "Age 35", "Age 40"),
                                    covariate.labels = c("log house price/income ratio", "log income", "prop. white", "prop. employed",
                                                         "prop. married", "Constant"),
                                    align=TRUE, type = "text", out = "tables/ownership_many_norm.html")

fwrite(mean_home_inc_add, "houses_dataset.csv")
