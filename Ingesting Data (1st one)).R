# load the required packages
library(data.table)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# define paths to compressed tsv files
root <- '/Users/komalmohanlal/Desktop/Final Thesis /Final Submission-Codes /Datasets/'
basics_path <- "title.basics.tsv.gz"
crew_path <- "title.crew.tsv.gz"
ratings_path <- "title.ratings.tsv.gz"
akas_path <- "title.akas.tsv.gz"
principals_path <- "title.principals.tsv.gz"

# ingest data
df_movies <- fread(paste0(root,basics_path), header = TRUE, quote = "", sep = "\t")
df_crew <- fread(paste0(root,crew_path), header = TRUE, quote = "", sep = "\t")
df_ratings <- fread(paste0(root,ratings_path), header = TRUE, quote = "", sep = "\t")
df_akas <- fread(paste0(root,akas_path), header = TRUE, quote = "", sep = "\t")
df_principals <- fread(paste0(root,principals_path), header = TRUE, quote = "", sep = "\t")

# keep only movies related data
df_movies <- subset(df_movies, titleType == "movie")
df_movies <- merge(df_movies, df_crew, by = "tconst", all = FALSE)
df_movies <- merge(df_movies, df_ratings, by = "tconst", all = FALSE)
rm(df_crew, df_ratings)
df_movie_ids <- data.frame(titleId = df_movies$tconst) 
df_akas <- merge(df_akas, df_movie_ids, by = "titleId", all = FALSE)
df_akas <- df_akas %>% rename(tconst = titleId)
df_movie_ids <- data.frame(tconst = df_movies$tconst)  
df_principals <- merge(df_principals, df_movie_ids, by = "tconst", all = FALSE)

# select relevant columns in each datasets
df_principals <- df_principals %>% select(-ordering, -job, -characters)
df_principals <- df_principals %>% rename(movieID = tconst, nameID = nconst, role = category)

df_movies <- df_movies %>% select(-titleType, -primaryTitle, -endYear, -directors)
df_movies <- df_movies %>% rename(movieID = tconst, title = originalTitle, releaseYear = startYear)

df_writers <- df_movies %>% select(movieID, writers)
df_genres <- df_movies %>% select(movieID, genres)
df_movies <- df_movies %>% select(-writers, -genres)

# transform writers and genres data
max_columns <- 10
df_writers_wide <- separate(df_writers, writers, into = paste0("writer", 1:max_columns), sep = ",", extra = "merge")
df_writers_wide <- df_writers_wide %>% select(movieID, writer1, writer2, writer3)
df_genres_wide <- separate(df_genres, genres, into = paste0("genre", 1:max_columns), sep = ",", extra = "merge")
df_genres_wide <- df_genres_wide %>% select(movieID, genre1, genre2, genre3)
rm(df_genres,df_writers)

# concat writers and geners information to movies dataset
df_movies <- merge(df_movies, df_writers_wide, by = "movieID", all = FALSE)
df_movies <- merge(df_movies, df_genres_wide, by = "movieID", all = FALSE)
rm(df_genres_wide, df_writers_wide, df_movie_ids)

# transform principals data
df_principals <- df_principals %>% group_by(movieID, role) %>% mutate(Rank = rank(nameID))
df_principals <- df_principals %>% mutate(rolePlayed = paste(role, Rank, sep = ""))
df_principals <- df_principals %>% ungroup() %>% select(movieID, nameID, rolePlayed)
df_principals_ranked <- pivot_wider(df_principals, names_from = rolePlayed, values_from = nameID)
df_principals_ranked <- df_principals_ranked %>% 
  select(movieID, actor1, actor2, actor3, actress1, actress2, actress3,director1, director2, director3, 
         producer1, producer2, producer3, editor1, editor2, editor3, composer1, composer2, composer3, 
         cinematographer1, cinematographer2, cinematographer3, self1, self2, self3)
rm(df_principals)

# concat principals information to movies dataset
df_movies <- merge(df_movies, df_principals_ranked, by = "movieID", all = FALSE)
rm(df_principals_ranked)

# prepare and save data
df_movies <- apply(df_movies,2,as.character)
write.csv(df_movies, file = paste0(root,'movies_data_1.csv'), row.names = FALSE)
write.csv(df_akas, file = paste0(root,'movies_data_2.csv'), row.names = FALSE)
