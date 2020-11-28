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

install.packages("tidyverse")
library(tidyverse)

install.packages("dplyr")
library(dplyr)

install.packages("devtools")
library(devtools)
devtools::install_github('charlie86/spotifyr')
library(spotifyr)
Sys.setenv(SPOTIFY_CLIENT_ID = '47fbca54d65e48b28bedb73631788a58')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '1c5a9b814c6b4fd281b17f27add79d4d')
access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))



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
tunefind_top_100_audio_features$duration_secs <- NULL 

# Convert "time_signature" from an integer variable to a factor.
factor(tunefind_top_100_audio_features$time_signature)

# Convert "key" into it's original symbols.
tunefind_top_100_audio_features$key <- as.character(tunefind_top_100_audio_features$key)
tunefind_top_100_audio_features$key <- revalue(tunefind_top_100_audio_features$key, c("0" = "C", "1" = "C♯,D♭", "2" = "D", "3" = 
                                                                                        "D♯,E♭", "4" = "E", "5" =  "F", "6" = "F♯,G♭","7" = "G","8" = "G♯,A♭","9" = "A","10" = "A♯,B♭","11" = "B"))


# Convert "mode" from an integer to a logical class.
tunefind_top_100_audio_features$mode <- as.logical(tunefind_top_100_audio_features$mode)



#### ANALYSIS OF THE DATA ####
# I'll be looking for trends and patterns in the audio features of these 100 songs.

library(ggplot2)
install.packages("ggplot2")
library(ggplot)

c
library(corrplot)


# Using the corrplot function, I can see the correlation between variables.

corr_tftop100 <- tunefind_top_100_audio_features[5:17]
mt_corr <- cor(corr_tftop100)
corrplot(mt_corr, method = "circle", type = "upper", tl.srt = 50)


# https://drive.google.com/file/d/1-A5p39P6hqelHGQzStp74SG3mxpXj0F8/view?usp=sharing 
# "Loudness" and "energy" are highly positively correlated.
# Also, danceability is positively correlated with "energy", "loudness" and "valence".
# "Energy" and "acousticness" are highlight negatively correlated (a lot of acoustic songs are slow-tempo ballads!).


# SinI can use a density plot to see how these three variables are distributed.

corr_density <- ggplot(tunefind_top_100_audio_features) +
  geom_density(aes(energy, fill ="energy", alpha = 0.1)) + 
  geom_density(aes(valence, fill ="valence", alpha = 0.1)) + 
  geom_density(aes(danceability, fill ="danceability", alpha = 0.1)) + 
  scale_x_continuous(name = "Energy, Valence and Danceability") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of Energy, Valence and Danceability") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")

corr_density

# https://drive.google.com/file/d/1-qESgq6fK3Wp-x0ru7txMd9jpbhmDqDX/view?usp=sharing 
# The distribution of these three variables are very similar to each other (limited between 0 and 1).




# I'll uncover the most common key(s) and tempo(s) among these songs.

song_keys <- tunefind_top_100_audio_features %>%
  group_by(key) %>%
  dplyr::summarise(n_key = n()) %>%
  arrange(desc(n_key))

song_keys$key <- factor(song_keys$key, levels = song_keys$key[order(song_keys$n_key)])

ggplot(song_keys, aes(x = reorder(key,-n_key), y = n_key, fill = reorder(key,-n_key))) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of the Keys", x = "Keys", y = "Count of Keys") +
  geom_text(aes(label=n_key), position = position_stack(vjust = 0.8)) +
  theme_bw() +
  theme(plot.title = element_text(size=15,face = "bold"), axis.title = element_text(size=12)) +
  theme(legend.position="none")


# https://drive.google.com/file/d/1Z4m_FdB7oApCmpCIuSXp_DDBqwLfqDEv/view?usp=sharing
# Looks like G, C and F are the most common keys.

tempo_density <- ggplot(tunefind_top_100_audio_features) +
  geom_density(aes(tempo, fill ="tempo")) + 
  scale_x_continuous(name = "Tempo") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of Tempo") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Paired")

tempo_density

# https://drive.google.com/file/d/1q1tjO7ldaDPLQvtHw4tNo6OET8UvhBJA/view?usp=sharing 
# Looks like most songs are hovering around 100-150 BPM. 


# In summary, "syncable" songs seems to have the following features:
# 1. Energy, Valence and Danceability with a value between 0 and 1.
# 2. G, C and F are the most common Keys.
# 3. Tempo is around 100-150 BPM. 


#### PREDICTING "SYNCABILITY" ####

# I'll use a Linear regression model to be able to predict if a song is "syncable" (works well for TV/film) to see if it matches my findings. 

# I'll also add a new binary variable "syncable" or "not syncable" and add additional data to run the model against. 

syncability <- tunefind_top_100_audio_features 
newcol <- data.frame(synced=("YES"))
syncability_new <- cbind(syncability, newcol)   
additional_data <- (read.csv("/Users/carlief/Desktop/additional_data.csv"))
newcol1 <- data.frame(synced=("NO"))

additional_data_new <- cbind(additional_data, newcol1)  

factor(additional_data_new$time_signature)
additional_data_new$key <- as.character(additional_data_new$key)
additional_data_new$key <- revalue(additional_data_new$key, c("0" = "C", "1" = "C♯,D♭", "2" = "D", "3" = 
                                                        "D♯,E♭", "4" = "E", "5" =  "F", "6" = "F♯,G♭","7" = "G","8" = "G♯,A♭","9" = "A","10" = "A♯,B♭","11" = "B"))

additional_data_new$mode <- as.logical(additional_data_new$mode)
additional_data_new$duration_secs <- NULL

syncable <- rbind(syncability_new, additional_data_new) 


# I'll split data set into training set and test set. 

syncable = syncable[-131,]
syncable$tempo <- as.integer(syncable$tempo)
syncable$energy <- as.numeric(syncable$energy)
syncable$danceability <- as.numeric(syncable$danceability)
syncable$valence <- as.numeric(syncable$valence)

n <- nrow(syncable)
ntrain <- round(n*0.6)
set.seed(56)
tindex <- sample(n, ntrain)

train_syncable <- syncable[tindex,]
test_syncable <- syncable[-tindex,]

newcol <- data.frame(isSyncable=(train_syncable$synced=="YES"))
train_syncable <- cbind(train_syncable, newcol) 

na.omit(train_syncable$tempo)
na.omit(test_syncable$tempo)


# ...and run the model! 

formula <- isSyncable ~ tempo + key + energy + valence + danceability
glm2 <- glm(formula, data=train_syncable, family="binomial")

summary(glm2)
coef(glm2)

summary(glm2)$coef


# Once trained, we can use the model to make predictions for the test set.

prob <- predict(glm2, newdata=test_syncable, type="response")
prob <- round(prob,3)*100

prob

