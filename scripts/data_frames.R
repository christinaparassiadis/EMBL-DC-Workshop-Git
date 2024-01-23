# data frames 
# import data
download.file(url = "https://ndownloader.figshare.com/files/2292169", 
              destfile="data_raw/portal_data_joined.csv")
library(tidyverse)
survey <- read_csv("data_raw/portal_data_joined.csv")

# info about table
head(survey)
view(survey)
str(survey)
dim(survey)
nrow(survey)
tail(survey)
names(survey)
summary(survey)

# indexing and subsetting
survey[1,6]
survey[1,]
survey[,1]
survey[c(1,2,3), c(5,6)]
survey[1:3, 5:6]
survey$plot_id

survey_200 <- survey[200,]
survey[nrow(survey),]
nrow(survey)/2
survey[nrow(survey)/2,]
