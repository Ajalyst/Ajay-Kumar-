library(dplyr)
library(tidyr)
library(ggplot2)

#========================================Dataset 1 Cleaning==================================


dataset1 <- read.csv('movies_data_1.csv')
head(dataset1)
tail(dataset1)

str(dataset1)

columns_to_drop <- c('writer2', 'writer3', 'actor2', 'actor3', 'actress2', 'actress3', 'director2', 'director3', 'producer1', 'producer2', 'producer3', 'editor1', 'editor2', 'editor3', 'composer1', 'composer2', 'composer3', 'cinematographer1', 'cinematographer2', 'cinematographer3', 'self1', 'self2', 'self3')
dataset1 <- dataset1[, !names(dataset1) %in% columns_to_drop]

head(dataset1)

colSums(is.na(dataset1))

# Replace '\N' with NA
dataset1$releaseYear[dataset1$releaseYear == '\\N'] <- NA
dataset1$runtimeMinutes[dataset1$runtimeMinutes == '\\N'] <- NA
dataset1$numVotes[dataset1$numVotes == '\\N'] <- NA
dataset1$genre1[dataset1$genre1 == '\\N'] <- NA
dataset1$writer1[dataset1$writer1 == '\\N'] <- NA

# Convert to numeric
dataset1$releaseYear <- as.numeric(dataset1$releaseYear)
year_mode <- mean(dataset1$releaseYear, na.rm = TRUE)
dataset1$runtimeMinutes <- as.numeric(dataset1$runtimeMinutes)
min_mode <- median(dataset1$runtimeMinutes, na.rm = TRUE)

# Compute modes
genre1_mode <- names(sort(table(dataset1$genre1), decreasing = TRUE))[1]
writer_mode <- names(sort(table(dataset1$writer1), decreasing = TRUE))[1]
actor_mode <- names(sort(table(dataset1$actor1), decreasing = TRUE))[1]
actress_mode <- names(sort(table(dataset1$actress1), decreasing = TRUE))[1]
director_mode <- names(sort(table(dataset1$director1), decreasing = TRUE))[1]

# Fill NA values
dataset1$releaseYear[is.na(dataset1$releaseYear)] <- year_mode
dataset1$runtimeMinutes[is.na(dataset1$runtimeMinutes)] <- min_mode
dataset1$genre1[is.na(dataset1$genre1)] <- genre1_mode
dataset1$genre2[is.na(dataset1$genre2)] <- 0
dataset1$genre3[is.na(dataset1$genre3)] <- 0
dataset1$writer1[is.na(dataset1$writer1)] <- writer_mode
dataset1$actor1[is.na(dataset1$actor1)] <- actor_mode
dataset1$actress1[is.na(dataset1$actress1)] <- actress_mode
dataset1$director1[is.na(dataset1$director1)] <- director_mode

colSums(is.na(dataset1))
tail(dataset1)

#========================================Dataset 2 Cleaning==================================

dataset2 <- read.csv('movies_data_2.csv')
head(dataset2)
tail(dataset2)

str(dataset2)

columns_to_drop <- c('ordering', 'types', 'attributes')
dataset2 <- dataset2[, !names(dataset2) %in% columns_to_drop]

head(dataset2)

colSums(is.na(dataset2))

# Replace '\N' with NA
dataset2$region[dataset2$region == '\\N'] <- NA
dataset2$language[dataset2$language == '\\N'] <- NA

# Compute modes
reg_mode <- names(sort(table(dataset2$region), decreasing = TRUE))[1]
lan_mode <- names(sort(table(dataset2$language), decreasing = TRUE))[1]

# Fill NA values
dataset2$region[is.na(dataset2$region)] <- reg_mode
dataset2$language[is.na(dataset2$language)] <- lan_mode

colSums(is.na(dataset2))

head(dataset2)

result <- dataset2 %>%
  group_by(tconst) %>%
  summarize(
    total_region = paste(unique(region), collapse = ", "),
    total_language = paste(unique(language), collapse = ", ")
  )

# Join the result back to the original dataframe
dataset2 <- dataset2 %>%
  left_join(result, by = "tconst")

head(dataset2)

new_dataframe <- dataset2[dataset2$isOriginalTitle == 1, ]
columns_to_drop <- c('region', 'language', 'isOriginalTitle')
new_dataframe <- new_dataframe[, !names(new_dataframe) %in% columns_to_drop]

head(new_dataframe)

colSums(is.na(new_dataframe))

tail(new_dataframe)

#========================================Checking==================================

head(dataset1)

tail(dataset1)

head(new_dataframe)

tail(new_dataframe)

#========================================Combining==================================

final_df <- merge(dataset1, new_dataframe, by.x = "movieID", by.y = "tconst", all = TRUE)

head(final_df)
tail(final_df)

final_df <- subset(final_df, select = -c(title.y))

str(final_df)

# Check for duplicated values in the movieID column
any(duplicated(final_df$movieID))

# Find duplicated movieID values
duplicated_movieIDs <- final_df$movieID[duplicated(final_df$movieID)]
duplicated_movieIDs

final_df <- final_df %>% distinct(movieID, .keep_all = TRUE)

# Check for duplicated values in the movieID column
any(duplicated(final_df$movieID))

head(final_df)
tail(final_df)

colSums(is.na(final_df))

final_df$num_regions <- sapply(strsplit(final_df$total_region, ","), function(x) length(x))
final_df$num_languages <- sapply(strsplit(final_df$total_language, ","), function(x) length(x))

head(final_df)

str(final_df)

colnames(final_df)

tail(final_df)

root <- '/Users/komalmohanlal/Desktop/Final Thesis /Final Submission-Codes /Datasets/'

write.csv(final_df, file = paste0(root,'dataset3.csv'), row.names = FALSE)
