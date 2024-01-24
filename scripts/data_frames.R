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

# Tools, Global options, never reload workspace, better to run script again 

# Factors
str(survey)
survey$sex <- factor(survey$sex)
levels(survey$sex)
nlevels(survey$sex)
sex <- factor(c("male", "female", "female", "male"))
sex <- factor(sex, levels = c("male", "female"))

sum(survey$taxa == "rabbit")
summary(survey$taxa)

# convert factors 
as.character(sex)

year_fct <- factor(c(1990,1983,1977,1997))
as.numeric(year_fct)
as.numeric(as.character(year_fct))
as.numeric(levels(year_fct))[year_fct]

# Renaming factors 
plot(survey$sex)
summary(survey$sex)
sex <- survey$sex
levels(sex)
sex <- addNA(sex)
levels(sex)[3] <- "undetermined"
levels(sex)
plot(sex)
levels(sex)[1:2] <- c("female", "male")
plot(sex)
sex <- factor(sex, levels=c("undeterminded", "female", "male"))
plot(sex)
