# install.packages(c("haven", "sas7bdat"))
# install.packages("tidyverse")
require(haven)
require(tidyverse)
library(dplyr)

df <- read_sas("us202104.sas7bdat")
dim(df) # 917 columns and 59 rows 


# Check that all data points belong to same country : US
if(unique(df$ctry) != "US"){
  stop("Data contins non US response")   
}

# Check that all data points belong to same wave of data collection : 2021-04
if(unique(df$waveno) != 8){
  stop("Data contains records of different date")   
}

# Check that all data points are have unique id
if(unique(duplicated(df$idno)) != "FALSE"){
  stop("Data contains duplicate values")   
}


hist(df$d_age_yr, xlab = "Age", xlim = c(0, 100))

# recode age catgory variable
df$age.lessthan24 = rep(0, nrow(df))
df$age.lessthan24[df$d_age_cat=="24 yr or younger"] = 1
df$age.2534 = rep(0, nrow(df))
df$age.2534[df$d_age_cat=="25-34 yr"] = 1
df$age.3544 = rep(0, nrow(df))
df$age.3544[df$d_age_cat=="35-44 yr"] = 1
df$age.4454 = rep(0, nrow(df))
df$age.4454[df$d_age_cat=="45-54 yr"] = 1
df$age.olderthan55 = rep(0, nrow(df))
df$age.olderthan55[df$d_age_cat=="55 yr or older"] = 1


# recode gender variable
df$gender.female = rep(0, nrow(df))
df$gender.female[df$d_gender=="FEMALE"] = 1

# 295 respondents have a child 18 years old or younger, and 622 do not.
table(df$havechild)

# 536 respondents have a parent over 50 years old and 381 do not. 
table(df$haveparent)

# These item were not included in this particular survey or have all missing values.
# df$v, df$vp
# TODO: Drop column said items above

# This item is only relevant for respondents with children
unique(df$vc)



# recode politics varibles
df$politics.rep = rep(0, nrow(df))
df$politics.rep[df$d_politics=="REP"] = 1
df$politics.dem = rep(0, nrow(df))
df$politics.dem[df$d_politics=="DEM"] = 2
df$politics.ind = rep(0, nrow(df))
df$politics.ind[df$d_politics=="IND"] = 3

# 7 rows do not have any values
count(df, d_politics) 
# drop rows with no records for d_politics column
df<-df[!(df$d_politics==""),]

model.vaccine = glm(vaxxed~d_age_yr+gender.female+politics.dem+politics.rep, 
                     data=df, family=binomial)
summary(model.vaccine)


