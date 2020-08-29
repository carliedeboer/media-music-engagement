# WHAT MAKES A SONG "SYNCABLE"?
# Carlie de Boer (she/her)
# www.carliedeboer.com
# Aug 2020



#### INTRO #### 
# As a music supervisor, I’ve always wanted to know what audio features of certain songs make them perfect for matching to film and TV A/V, which some in the industry call “syncable”. 

# In this project, I’ll be using TuneFind’s Top 100 Songs of 2019 list merged with Spotify’s API to highlight common patterns behind the audio features of these songs as well as build a tool that can analyze a song for it’s “sync-ability”.



#### BUILDING THE DATASET ####
# I was lucky enough to obtain a dataset from Tunefind (www.tunefind.com), which is an industry standard index of music and songs appearing in popular television shows and movies.  

# I'll use this (which contains Spotify track IDs) to obtain audio features from the Spotify API and add those to the dataset for analysis.

#### 
# Call the Spotify API.

install.packages("devtools")
library(devtools)
devtools::install_github('charlie86/spotifyr')
library(spotifyr)
Sys.setenv(SPOTIFY_CLIENT_ID = '47fbca54d65e48b28bedb73631788a58')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '1c5a9b814c6b4fd281b17f27add79d4d')
access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))

install.packages('tidyverse')
library(tidyverse)

install.packages('dplyr')
library(dplyr)


# DATA SET: Tunefind Top 100 Songs of 2019 ####
tunefind_top_100 <- read.csv('/Users/carlief/Desktop/tf_top100_2019.csv')
head(tunefind_top_100)


# Run lapply() on Tunefind data to populate dataset with audio features from Spotify.
tunefind_id <- pull(tunefind_top_100, 6)
as.character(tunefind_id)

spotify_audio_features <- lapply(tunefind_id, get_track_audio_features) %>% bind_rows()

## 3. Combined the two datasets to start analysis.
tunefind_top_100_audio_features <- cbind(tunefind_top_100, spotify_audio_features)



#### CLEANING THE DATA #### 
# To clean the data, I'll perform the following:

head(tunefind_top_100_audio_features)
summary(tunefind_top_100_audio_features)

# Remove unnecessary redundant variables. 
tunefind_top_100_audio_features$air_date <- NULL 
tunefind_top_100_audio_features$link <- NULL 
tunefind_top_100_audio_features$type <- NULL 
tunefind_top_100_audio_features$id <- NULL 
tunefind_top_100_audio_features$uri <- NULL 
tunefind_top_100_audio_features$track_href <- NULL 
tunefind_top_100_audio_features$analysis_url <- NULL 

# Convert "duration_ms" variable to seconds rather than milliseconds and rename "duration_secs".
tunefind_top_100_audio_features$duration_ms <- round(tunefind_top_100_audio_features$duration_ms / 1000)
colnames(tunefind_top_100_audio_features)[17] <- "duration_secs"

# Convert "key" and "time_signature" from an integer variable to a factor.
factor(tunefind_top_100_audio_features$key)
factor(tunefind_top_100_audio_features$time_signature)

# Convert "mode" from an integer to a logical class.
tunefind_top_100_audio_features$mode <- as.logical(tunefind_top_100_audio_features$mode)


## DO I NEED TO CONVERT ALL THE DECIMALS, SMALL NUMBERS UP?? ## 



#### ANALYSIS OF THE DATA ####
# I'll be looking for trends and patterns in the audio features of these 100 songs.

# CORRPLOT
# Use the corrplot function, I can see the correlation between variables.


# COMMON FEATURES
# Distribution of keys
# Density Plots of Correlated Variables
# Density Plot of Loudness



#### PREDICTING "SYNCABILITY" ####




