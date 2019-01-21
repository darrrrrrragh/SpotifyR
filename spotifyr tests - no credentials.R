############## Spotify Credentials ######################
Sys.setenv(SPOTIFY_CLIENT_ID = '')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '')
username <- ''


############## Librarys required ###########
library(spotifyr)
library(dplyr)
library(ggplot2)
library(reshape2)

############# Data Munging ###############

access_token <- get_spotify_access_token()


playlists <- 
  get_user_audio_features(username)

#2017 playlist
TopSongs2017 <- filter(playlists, playlist_name=='Top Songs 2017 Dupe')
TopSongs2017 <- within(TopSongs2017, Track_Artist <- paste(track_name, artist_name, sep=" - "))
TopSongs2017$year <- as.factor(2017)

#2016 playlist
TopSongs2016 <- filter(playlists, playlist_name=='Top Songs 2016 Dupe')
TopSongs2016 <- within(TopSongs2016, Track_Artist <- paste(track_name, artist_name, sep=" - "))
TopSongs2016$year <- as.factor(2016)

#all songs
AllSongs <- rbind(TopSongs2016, TopSongs2017) # concatenate all tracks

#all features with same scale (0-1)
AllSongs.f <- select(AllSongs, danceability, energy, energy, speechiness, acousticness, instrumentalness, liveness, valence, year)

AllSongs.m <- melt(AllSongs.f, id.var = "year")

#boxplots of features of similar scales by year
ggplot(data = AllSongs.m) +
  geom_boxplot(mapping = aes(x=variable, y=value, fill=year)) +
  coord_cartesian(ylim = c(0, 1))+ # all graphs from 0-1, no zooming in
  facet_wrap( ~ variable, scales = "free") # all separate graphs, not all on one

# no obvious skew for valence vs speed of song, dependent on major or minor
ggplot(data = TopSongs2017) +
  geom_smooth(mapping = aes(x = tempo, y = valence, color = mode)) # alpha, shape, size

#For lols - using colour as just a color (outside of aes argument)
ggplot(data = TopSongs2017) +
  geom_point(mapping = aes(x = tempo, y = valence), color = "blue") 


ggplot(data = TopSongs2017) +
  geom_point(mapping = aes(x = duration_ms, y = valence, color = mode))

ggplot(data = TopSongs2017) +
  geom_boxplot(mapping = aes(x=key, y=danceability))+
  labs(title="test", x="Key of track", y="Danceability")

ggplot(data = TopSongs2017, aes(x=reorder(Track_Artist, valence), y=valence))+
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Tracks by Valence", x="Track Name", y="Valence")

ggplot(data = TopSongs2017, aes(x=reorder(Track_Artist, valence), y=valence))+
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Tracks by Valence", x="Track Name", y="Valence")

## across year differences
ggplot(data = AllSongs) +
  geom_boxplot(mapping = aes(x=year, y=danceability, color = year))+
  labs(title="test", x="Year", y="Danceability")


################# testing k means with dataset #################

# Initialize total within sum of squares error: wss
wss <- 0


AllKmeans <- select(AllSongs.f, danceability, energy, energy, speechiness, acousticness, instrumentalness, liveness, valence)

# Look over 1 to 15 possible clusters
for (i in 1:15) {
  # Fit the model: km.out
  km.out <- kmeans(AllKmeans, centers = i, nstart = 20, iter.max = 50)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Select number of clusters
k <- 4

# Build model with k clusters: km.out
km.out <- kmeans(AllKmeans, centers = k, nstart = 20, iter.max = 50)

# View the resulting model
km.out


head(AllKmeans)

# Plot of Defense vs. Speed by cluster membership
plot(AllSongs.f[, c("danceability", "instrumentalness")],
     col = km.out$cluster,
     main = paste("k-means clustering of Tracks with", k, "clusters"),
     xlab = "danceability", ylab = "energy")


a <- TopSongs2017 %>%
  group_by(track_name) %>%
  arrange(desc(danceability))


plot(TopSongs2017$danceability)

plot(TopSongs2017$key)

TopSongs2017[10,]


key <- table(TopSongs2017$key) #summary table for key
barplot(key)

mode <- table(TopSongs2017$mode)
barplot(mode)

key_mode <- table(TopSongs2017$key_mode)
barplot(key_mode)

time_sign <- table(TopSongs2017$time_signature)
barplot(time_sign)

boxplot(TopSongs2017$duration_ms)

#Get average valence per artist
TopSongs2017 %>%
  group_by(artist_name) %>%
  summarise(avg_valence = mean(valence, na.rm = TRUE)) %>%
  arrange(desc(avg_valence))
  

#Get average speechiness and tempo per key
TopSongs2017 %>%
  group_by(key) %>%
  summarise_each(funs(mean), speechiness, tempo) %>%
  arrange(desc(speechiness))

#Get Min and Max danceability of each artist
TopSongs2017 %>%
  group_by(artist_name) %>%
  summarise_each(funs(min(., na.rm =TRUE), max(., na.rm=TRUE)), danceability)
