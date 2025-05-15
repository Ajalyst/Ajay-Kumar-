library(fuzzyjoin)
library(lares)
library(dplyr)
library(car)
library(tidyverse)
library(fuzzyjoin)
library(stringi)
library(ggplot2)
library(lares)
library(knitr)
library(lares)

#Loading data
imdb_data <- read.csv('dataset3.csv')
budget_data <- read.csv("LifetimeGross2023.csv")

#check  sample records 
head(imdb_data)
colSums(is.na(imdb_data))

head(budget_data)
colSums(is.na(budget_data))

#check for total record counts 
nrow(imdb_data)
nrow(budget_data)

##Change MovieTitles to lower case 
imdb_data <- imdb_data %>%
  mutate(title.x = tolower(title.x))

budget_data <- budget_data%>%
  mutate(title = tolower(title))

# Remove duplicates in imdb_data_final
imdb_data_final_unique <- imdb_data[!duplicated(imdb_data[c("title.x", "releaseYear")]), ]
nrow(imdb_data_final_unique)

# Remove duplicates in budget_data_final
budget_data_final_unique <- budget_data[!duplicated(budget_data[c("title", "year")]), ]
nrow(budget_data_final_unique)

head(imdb_data_final_unique)

# Merge datasets based on title and year
merged_data <- merge(imdb_data_final_unique, 
                     budget_data_final_unique, 
                     by.x = c("title.x", "releaseYear"), 
                     by.y = c("title", "year"))

# Display results
head(merged_data)
nrow(merged_data)
colSums(is.na(merged_data))

merged_data$lifetimegross <-  as.numeric(gsub("[\\$,]", "", merged_data$lifetimegross))

sorted_merged_data <- merged_data %>% arrange(rank)
head(sorted_merged_data)

str(merged_data)

write.csv(merged_data, file = "/Users/komalmohanlal/Desktop/Final Thesis /Final Submission-Codes /Datasets/dataset4.csv", row.names = FALSE)
