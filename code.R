# Creating Deep Audience Connections Beyond the Screen Through the Power of Music
# Carlie de Boer
# Dec 2020


# DATASET 
# Spotify API 
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

# Tunefind Top 100 Songs of 2019
tunefind_top_100 <- read.csv("https://raw.githubusercontent.com/carliedeboer/syncability/master/tunefind_top_100.csv")
head(tunefind_top_100)

tunefind_id <- pull(tunefind_top_100, 6)
as.character(tunefind_id)

spotify_audio_features <- lapply(tunefind_id, get_track_audio_features) %>% bind_rows()

# Combined datasets
tunefind_top_100_audio_features <- cbind(tunefind_top_100, spotify_audio_features)



# QUANTITATIVE ANALYSIS

head(tunefind_top_100_audio_features)
summary(tunefind_top_100_audio_features)

tunefind_top_100_audio_features$air_date <- NULL 
tunefind_top_100_audio_features$link <- NULL 
tunefind_top_100_audio_features$type <- NULL 
tunefind_top_100_audio_features$id <- NULL 
tunefind_top_100_audio_features$uri <- NULL 
tunefind_top_100_audio_features$track_href <- NULL 
tunefind_top_100_audio_features$analysis_url <- NULL 
tunefind_top_100_audio_features$duration_secs <- NULL 

factor(tunefind_top_100_audio_features$time_signature)

install.packages("plyr")
library(plyr)

# Convert "key" into it's original symbols
tunefind_top_100_audio_features$key <- as.character(tunefind_top_100_audio_features$key)
tunefind_top_100_audio_features$key <- revalue(tunefind_top_100_audio_features$key, c("0" = "C", "1" = "C♯,D♭", "2" = "D", "3" = 
                                                                                        "D♯,E♭", "4" = "E", "5" =  "F", "6" = "F♯,G♭","7" = "G","8" = "G♯,A♭","9" = "A","10" = "A♯,B♭","11" = "B"))
# Convert "mode" from an integer to a logical class
tunefind_top_100_audio_features$mode <- as.logical(tunefind_top_100_audio_features$mode)


install.packages("ggplot2")
library(ggplot2)

library(corrplot)


# Pearson correlation coefficient (PCC) for key audio features
corr_tftop100 <- tunefind_top_100_audio_features[5:17]
mt_corr <- cor(corr_tftop100)
corrplot(mt_corr, method = "circle", type = "upper", tl.srt = 50)


# Kernel density estimation (KDE) for key audio features
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


# Pearson correlation coefficient (PCC) of common keys
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
  theme(legend.position="none"


# Destiny plot of tempos
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


# Multivariable Logistic Regression
syncability <- tunefind_top_100_audio_features 
newcol <- data.frame(synced=("YES"))
syncability_new <- cbind(syncability, newcol)   
additional_data <- (read.csv("https://raw.githubusercontent.com/carliedeboer/syncability/master/additional_data.csv"))
newcol1 <- data.frame(synced=("NO"))

additional_data_new <- cbind(additional_data, newcol1)  

factor(additional_data_new$time_signature)
additional_data_new$key <- as.character(additional_data_new$key)
additional_data_new$key <- revalue(additional_data_new$key, c("0" = "C", "1" = "C♯,D♭", "2" = "D", "3" = 
                                                        "D♯,E♭", "4" = "E", "5" =  "F", "6" = "F♯,G♭","7" = "G","8" = "G♯,A♭","9" = "A","10" = "A♯,B♭","11" = "B"))
additional_data_new$mode <- as.logical(additional_data_new$mode)

additional_data_new$duration_secs <- NULL
syncability_new$duration_ms <- NULL

syncable <- rbind(syncability_new, additional_data_new) 


# Split into a training set and test set 
str(syncable)

syncable = syncable[-131,]
syncable$tempo <- as.integer(syncable$tempo)
syncable$energy <- as.numeric(syncable$energy)
syncable$danceability <- as.numeric(syncable$danceability)
syncable$valence <- as.numeric(syncable$valence)
syncable$loudness <- as.numeric(syncable$loudness)
syncable$liveness <- as.numeric(syncable$liveness)

n <- nrow(syncable)
ntrain <- round(n*0.6)
set.seed(56)
tindex <- sample(n, ntrain)

train_syncable <- syncable[tindex,]
test_syncable <- syncable[-tindex,]

newcol <- data.frame(isSyncable=(train_syncable$synced=="YES"))
train_syncable <- cbind(train_syncable, newcol) 


# Run the model
formula <- isSyncable ~ key + danceability + loudness
glm2 <- glm(formula, data=train_syncable, family="binomial")

summary(glm2)
coef(glm2)

summary(glm2)$coef


# Test set
prob <- predict(glm2, newdata=test_syncable, type="response")
prob <- round(prob,3)*100

# Fit plots
plot(glm2)

