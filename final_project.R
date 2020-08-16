# EXPLORE AND CLEAN THE DATA

spotify_songs <- read.csv('file:///Users/carlief/Desktop/spotify_songs.csv')
head(spotify_songs)
summary(spotify_songs)

# To clean the data, I'll perform the following:

# 1. Rename "name" to "songs" for clarity and
# 2. Change "duration_ms" variable to seconds rather than milliseconds.
# 2. Change "key" into a factor 1-11.
# 3. Re-categorize explicit, mode and popularity into TRUE or FLASE.
# 4. Change "release_date" and "year" to data and time variables.

colnames(spotify_songs)[13] <- "songs"
spotify_songs$duration_ms <- round(spotify_songs$duration_ms / 1000)
factor(spotify_songs$key)
