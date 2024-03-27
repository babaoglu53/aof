library(RCurl)
library(readr)
library(dplyr)
library(tidyverse)

x <- getURL("https://raw.githubusercontent.com/sharmaroshan/Students-Performance-Analytics/master/StudentsPerformance.csv")
data <- read.csv(text = x)

head(data)

class(data)

dim(data)

nrow(data)

ncol(data)

colnames(data)

getwd()

dir()

ls()

data = dplyr::rename(data, GENDER=gender)

names(data)

data_upper = dplyr::rename_all(data, toupper)

head(data_upper)

pull(dplyr::select(data, GENDER))

filter(data, GENDER=="male")

data = mutate(data, new.score=math.score/2)

head(data)

arrange(data, desc(reading.score))

transmute(data, newcol=writing.score*2, math.score, reading.score)
