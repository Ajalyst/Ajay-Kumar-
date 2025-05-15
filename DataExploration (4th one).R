library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(car)
library(tidyverse)
library(fuzzyjoin)
library(lares)
library(knitr)


movies <- read.csv('dataset4.csv')
head(movies)


#================================Regional Analysis====================================

ggplot(movies, aes(x = as.factor(num_languages))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Count of Movies by Number of Languages", x = "Number of Languages", y = "Count") +
  theme_minimal()

ggplot(movies, aes(x = as.factor(num_regions))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Count of Movies by Number of Regions", x = "Number of Regions", y = "Count") +
  theme_minimal()

regions <- unlist(strsplit(as.character(movies$total_region), ", "))
movies_count <- data.frame(region = regions)
movies_count$region <- factor(movies_count$region)  
movies_count <- data.frame(table(movies_count$region))
colnames(movies_count) <- c("region", "count")

top_regions <- head(movies_count[order(-movies_count$count), ], 10)

ggplot(top_regions, aes(x = region, y = count, fill = region)) + geom_bar(stat = "identity") + labs(title = "Top 10 Regions with the Most Movies Released", x = "Region", y = "Number of Movies") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#==================================== Outlier======================

# remove outlier values
numeric_cols <- c('runtimeMinutes','numVotes','num_regions','num_languages')
for (col_name in numeric_cols) {
  percentile_01 <- quantile(movies[[col_name]], 0.001, na.rm = TRUE)
  percentile_99 <- quantile(movies[[col_name]], 0.999, na.rm = TRUE)
  movies <- movies[movies[, col_name] > percentile_01, ]
  movies <- movies[movies[, col_name] < percentile_99, ]
}

#================================= Runtime Analysis ======================

# visualize the distribution of 'runtimeMinutes'
ggplot(movies, aes(x = runtimeMinutes)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Distribution of runtimeMinutes",
       x = "runtimeMinutes",
       y = "Frequency")

ggplot(movies, aes(x = runtimeMinutes, y = averageRating)) + geom_point(color = "skyblue") + labs(title = "Scatter Plot of Runtime vs Average Rating", x = "Runtime (Minutes)", y = "Average Rating") + theme_minimal()

# optimal runtime for successful movies 
high_rated_movies <- subset(movies, averageRating >= 7)
ggplot(high_rated_movies, aes(x = runtimeMinutes, y = averageRating)) + geom_smooth(method = "loess", se = FALSE, color = "blue") + labs(title = "Relationship between Runtime and Average Rating", x = "Runtime (Minutes)", y = "Average Rating") + theme_minimal()

#===================================Average Rating Analysis======================

ggplot(movies, aes(x = averageRating)) +
  geom_histogram(binwidth = 0.3, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average Ratings",
       x = "Average Ratings",
       y = "Frequency")

ggplot(movies, aes(x = genre1, y = averageRating, fill = genre1)) +
  geom_boxplot() +
  labs(title = "Boxplot of Average Rating by Genre",
       x = "Genre",
       y = "Average Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

movies$title_length <- nchar(movies$title.x)
length_impact <- aggregate(averageRating ~ title_length, data = movies, FUN = median)

ggplot(length_impact, aes(x = title_length, y = averageRating)) + geom_line(color = "skyblue") + geom_point() + labs(title = "Impact of Title Length on Average Rating", x = "Title Length", y = "Average Rating") + theme_minimal()

#Statistical Validation

linear_model <- lm(averageRating ~ title_length, data = movies)
ggplot(movies, aes(x = title_length, y = averageRating)) + geom_point(color = "skyblue") + geom_smooth(method = "lm", se = FALSE, color = "black") + labs(title = "Impact of Title Length on Average Rating", x = "Title Length", y = "Average Rating") + theme_minimal()

#====================================Genre Analysis======================

ggplot(movies, aes(y = as.factor(genre1))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Count of 1st Genres", x = "Genres", y = "Count") +
  theme_minimal()

top_genres <- unique(movies$genre1[order(-movies$averageRating)])[1:5]
subset_data <- subset(movies, genre1 %in% top_genres)

ggplot(subset_data, aes(x = releaseYear, y = averageRating, color = genre1)) + geom_smooth(method = "loess", se = FALSE, aes(group = genre1)) + labs(title = "Average Rating of Top 5 Genres Over Years", x = "Release Year", y = "Average Rating", color = "Genre") + theme_minimal()

ggplot(movies, aes(x = genre1, y = runtimeMinutes)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Runtime Minutes by Genre",
       x = "Genre",
       y = "Runtime Minutes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#================================== Yearly Analysis ======================

ggplot(movies, aes(x = releaseYear, y = lifetimegross)) + geom_point(color = "skyblue") +
  labs(
    title = "Distribution of Lifetime Gross Revenue",
    x = "Release Year",
    y = "Lifetime Gross"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()