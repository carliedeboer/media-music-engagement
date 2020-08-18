# WHAT MAKES A SONG "SYNCABLE"?
# Carlie de Boer (she/her)
# www.carliedeboer.com
# Aug 2020


# INTRO
############################

# As a music supervisor, I’ve always wanted to know what audio features of certain songs make them perfect for matching to film and TV A/V, which some in the industry call “syncable”. 

# In this project, I’ll be using TuneFind’s Top 100 Songs of 2019 list merged with Spotify’s dataset of 160k+ songs released from 1921 to 2020 to highlight common patterns behind 
# the audio features of these songs as well as build a tool that can analyze a song for it’s “sync-ability”.


# EXPLORING THE DATA
############################

# To clean the data, I'll perform the following:


# DATASET 1 - SPOTIFY

spotify_songs <- read.csv('file:///Users/carlief/Desktop/spotify_songs.csv')
head(spotify_songs)
summary(spotify_songs)

# 1. Rename "name" to "songs" for clarity.
# 2. Convert "duration_ms" variable to seconds rather than milliseconds and rename "duration_secs"
# 2. Convert "key" from an integer variable to a factor.
# 3. Convert explicit, mode and popularity from an integer to a logical class.
# 4. Delete "release_date" since it's redundant with "year".

colnames(spotify_songs)[13] <- "songs"

spotify_songs$duration_ms <- round(spotify_songs$duration_ms / 1000)
colnames(spotify_songs)[4] <- "duration_secs"

factor(spotify_songs$key)

spotify_songs$explicit <- as.logical(spotify_songs$explicit)
spotify_songs$mode <- as.logical(spotify_songs$mode)
spotify_songs$popularity <- as.logical(spotify_songs$popularity)

spotify_songs$release_date <- NULL


# DATASET 2 - TUNEFIND

tunefind_100songs <- read.csv('file:///Users/carlief/Desktop/tunefind_100songs.csv')

# Now, I'll merge the two datasets  based on ID:

merge()


# ANALYSIS
############################

# I'll be looking for trends and patterns in the audio features of these 100 songs.


# CORRPLOT
# Use the corrplot function, I can see the correlation between variables.


# COMMON FEATURES
# Distribution of keys
# Density Plots of Correlated Variables
# Density Plot of Loudness



# PREDICTING "SYNCABILITY"
############################




