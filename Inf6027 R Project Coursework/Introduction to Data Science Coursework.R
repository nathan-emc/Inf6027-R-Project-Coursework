#install packages if not done already
install.packages("tidyverse")
install.packages("nortest")
install.packages("ggcorrplot")
install.packages("car")
install.packages("Hmisc")
install.packages("olsrr")
install.packages("MASS")
install.packages("caret")
install.packages("broom")

#load all necessary packages
library(tidyverse)
library(jsonlite)
library(ggcorrplot)
library(car)
library(Hmisc)
library(olsrr)
library(nortest)
library(lattice)
library(caret)
library(broom)


#Reading csv files
artistData <- read_delim("artists.csv", delim = "\t")
songAcousticFeatures <- read_delim("acoustic_features.csv", delim = "\t")
songData <- read_delim("songs.csv", delim = "\t")

#merging songAcousticFeatures and songData
combinedSongData <- merge(songAcousticFeatures, songData, by = "song_id")

#defining a function to delete all problematic characters in the artist column of combinedSongData, as well as get a list of all artist ID's
parse_artists <- function(artists_string) {
  clean_string <- gsub("['\"(){}]", "", artists_string) #replaces useless characters with nothing
  clean_string <- gsub(": ", ":", clean_string) #replaces ": " with ":"
  
  artist_pairs <- str_split(clean_string, ",\\s*") [[1]] #seperate different artists
  
  artist_ids <- sapply (artist_pairs, function(pair) { #loops through each Artist name and Artist ID and only extracts the ID
    str_split(pair, ":") [[1]][1] #splits each pair by the colon, and retrieves the first part (the artist ID)
  })
  return(artist_ids) #returns the artist's id's
} 

#apply the function to the combined song data
artist_ids_list <- lapply(combinedSongData$artists, parse_artists) #creates a list of the number of artists for each song

#create 4 columns for each song, to put separate artist ID's in depending on the number of artists on the track
for (i in seq_len(4)) { #creates a sequence from 1, to 4
  combinedSongData[[paste0("artist", i, "ID")]] <- sapply(artist_ids_list, function(ids) { 
    if (length(ids) >= i) ids[i] else NA  # Add artist ID if it exists, otherwise NA
  })
}

##adding genres to combinedSongData, from artistData
#get rid of useless characters
artistData$clean_genres <- gsub("[\\[\\]']", "", artistData$genres)

#create a function that gets all the artists genres
get_genres <- function(ids) {
  artist_genres <- artistData$clean_genres[artistData$artist_id %in% ids]
  return(paste(artist_genres, collapse = ", "))
}

#add the genres to each song
combinedSongData$genres <- sapply(1:nrow(combinedSongData), function(i) {
  # Collect all artist IDs for the current song (ignoring NA values)
  artist_ids <- na.omit(c(combinedSongData$artist1ID[i], 
                          combinedSongData$artist2ID[i], 
                          combinedSongData$artist3ID[i], 
                          combinedSongData$artist4ID[i]))
  
  # Get the genres for these artist IDs
  return(get_genres(artist_ids))
})

# Remove square brackets and single quotes
combinedSongData$genres <- gsub("\\[|\\]", "", combinedSongData$genres)  # Remove brackets
combinedSongData$genres <- gsub("'", "", combinedSongData$genres)  # Remove single quotes

# Remove any leading/trailing spaces
combinedSongData$genres <- trimws(combinedSongData$genres)

# Split the genres string by commas and convert it into a list
combinedSongData$genres <- strsplit(combinedSongData$genres, ",\\s*")

#remove any duplicates in each song
combinedSongData$genres <- lapply(combinedSongData$genres, unique)

#see which genres are most popular
allGenres <- unlist(combinedSongData$genres)

# Count the frequency of each genre
genreCounts <- table(allGenres)
 
#Create Data Frame with only the variables we need; and remove all missing values for any acoustic features and popularity
acousticFeaturesPopularityGenresDF <- combinedSongData %>%
  select(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence, tempo, popularity, genres) %>%
  filter(complete.cases(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence, tempo, popularity))


# Calculate z-scores for each variable
z_scores <- scale(acousticFeaturesPopularityGenresDF[, c("acousticness", "danceability", "energy", 
                                                   "instrumentalness", "liveness", "loudness", 
                                                   "speechiness", "valence", "tempo", "popularity")])

# Identify rows with any Z-score greater than 3 or less than -3
outliers <- apply(z_scores, 1, function(x) any(abs(x) > 3))

# Remove rows where any variable has a Z-score > 3 or < -3
acousticFeaturesPopularityGenresDF <- acousticFeaturesPopularityGenresDF[!outliers, ]

#remove genres
acousticFeaturesPopularityDF <- acousticFeaturesPopularityGenresDF %>%
  select(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence, tempo, popularity)


# Create a correlation matrix
cor_matrix <- cor(acousticFeaturesPopularityDF)

# Visualize the correlation matrix
ggcorrplot(cor_matrix, lab = TRUE, lab_size = 3, colors = c("green", "white", "purple"))

# Compute correlation matrix with p-values
cor_results <- rcorr(as.matrix(acousticFeaturesPopularityDF))

print(cor_results)


# Fit the regression model for assumption testing
modAllGenresAssumptions <- lm(
  formula=popularity~acousticness + danceability + energy + instrumentalness + liveness + loudness + speechiness + valence + tempo,
  data=acousticFeaturesPopularityDF
)

#Summary of model
summary(modAllGenresAssumptions)

# Calculate VIF values
vif_values <- vif(modAllGenresAssumptions)

# Print VIF values
print(vif_values)

#check for residuals
plot(modAllGenresAssumptions, which = 1)

#create a Q-Q plot
plot(modAllGenresAssumptions, which = 2)

#Create a data frame for pop songs
popDataFrame <- acousticFeaturesPopularityGenresDF %>%
  filter(map_lgl(genres, ~any(str_detect(.x, "pop")))) %>%
  select(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence, popularity)

#Create a data frame for rock songs
rockDataFrame <- acousticFeaturesPopularityGenresDF %>%
  filter(map_lgl(genres, ~any(str_detect(.x, "rock|mellow gold")))) %>%
  select(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence, popularity)

#Create a data frame for hip hop songs
hiphopDataFrame <- acousticFeaturesPopularityGenresDF %>%
  filter(map_lgl(genres, ~any(str_detect(.x, "hip hop|rap"))))%>%
  select(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence, popularity)

# Function to create regression model with a training data set and a test data set, using 90% training data and 10% test data
testRegression <- function(formula, data) {
  # Create a 90/10 split (90% training, 10% testing)
  set.seed(123) # For reproducibility
  trainIndex <- createDataPartition(data$popularity, p = 0.9, list = FALSE)
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]
  
  # Fit the model on training data
  model <- lm(formula, data = trainData)
  
  return(list(
  summary (model),
  
  head(testData)))
}

#test regression for all genres
testRegression ( formula=popularity~acousticness + danceability + energy + instrumentalness + liveness + loudness + speechiness + valence,
                 data=acousticFeaturesPopularityDF
)

#test regression for Pop songs
testRegression ( formula=popularity~acousticness + danceability + energy + instrumentalness + liveness + loudness + speechiness + valence,
                 data=popDataFrame
)

#test regression for rock songs
testRegression ( formula=popularity~acousticness + danceability + energy + instrumentalness + liveness + loudness + speechiness + valence,
                 data=rockDataFrame
)

#test regression for hip hop songs
testRegression ( formula=popularity~acousticness + danceability + energy + instrumentalness + liveness + loudness + speechiness + valence,
                 data=hiphopDataFrame
)

## Fitting Regression models including test data
#Fit the new Regression Model
modAllGenres <- lm(
  formula=popularity~acousticness + danceability + energy + instrumentalness + liveness + loudness + speechiness + valence,
  data=acousticFeaturesPopularityDF
)

#See the Regression Model
summary(modAllGenres)

#See 95% confidence intervals
confint(modAllGenres, level = 0.95)

#Create a regression model using only pop songs
modPop <- lm(
  formula=popularity~acousticness + danceability + energy + instrumentalness + liveness + loudness + speechiness + valence,
  data=popDataFrame
)

#See the regression model
summary(modPop)

#See 95% confidence intervals
confint(modPop, level = 0.95)



#Create a regression model using only rock songs
modRock <- lm(
  formula=popularity~acousticness + danceability + energy + instrumentalness + liveness + loudness + speechiness + valence,
  data=rockDataFrame
)

#See the regression model
summary(modRock)

#See 95% confidence intervals
confint(modRock, level = 0.95)


#Create a regression model using only hip hop
modHiphop <- lm(
  formula=popularity~acousticness + danceability + energy + instrumentalness + liveness + loudness + speechiness + valence,
  data=hiphopDataFrame
)

#See the regression model
summary(modHiphop)

#See 95% confidence intervals
confint(modHiphop, level = 0.95)

#Create a data frame with all the coefficients of each regression model
coefficientsModAllGenres <- tidy(modAllGenres) %>%
  select(term, estimate) %>%
  rename('Predictor' = term, 'All Genres' = estimate)
coefficientsPop <- tidy(modPop) %>%
  select(term, estimate) %>%
  rename('Predictor' = term, 'Pop' = estimate)
coefficientsRock <- tidy(modRock) %>%
  select(term, estimate) %>%
  rename('Predictor' = term, 'Rock' = estimate)
coefficientsHiphop <- tidy(modHiphop) %>%
  select(term, estimate) %>%
  rename('Predictor' = term, 'Hip Hop' = estimate)

coefficientsDF <- merge (coefficientsModAllGenres, coefficientsPop, by = "Predictor")
coefficientsDF <- merge (coefficientsDF, coefficientsRock, by = "Predictor")
coefficientsDF <- merge (coefficientsDF, coefficientsHiphop, by = "Predictor")

coefficientsDF <- coefficientsDF %>%
  filter(Predictor != "(Intercept)")

#create a data frame that is able to become a visualisation
coefficientsLong <- coefficientsDF %>%
  pivot_longer(
    cols = -Predictor,             # Columns to pivot (all except 'Predictor')
    names_to = "Outcome",          # Name for the new "Outcome" column
    values_to = "Coefficient"      # Name for the new "Coefficient" column
  ) %>%
  mutate(Predictor = str_to_title(Predictor))

#Convert all coefficients to 2.d.p.
coefficientsLong$Coefficient <- round(coefficientsLong$Coefficient, 2)

#arrange the coefficients data frame by Outcome
coefficientsLong <- coefficientsLong %>%
  arrange(Outcome)

# Manually add the 'Significant' column based on known significance for each row
coefficientsLong$Significant <- c(
  "***", 
  "***",
  "",   
  "***", 
  "***", 
  "***", 
  "***",
  "***", 
  "***", 
  "",     
  "***", 
  "***",  
  "***", 
  "***",  
  "",    
  "***", 
  "***", 
  "***", 
  "",    
  "***", 
  "***",  
  "***",
  "**", 
  "***", 
  "***",  
  "***", 
  "",  
  "",    
  "***", 
  "***", 
  "",    
  "***"  
)

#combine the significance and coefficient columns
coefficientsLong$Coefficient <- paste0(coefficientsLong$Coefficient, coefficientsLong$Significant)


#create a matrix of coefficients, with the x axis being predictor variables, and the y axis being outcome variables.
ggplot(coefficientsLong, aes(x = Predictor, y = Outcome)) +
  geom_tile(fill = "darkseagreen4", color = "white") +
  geom_text(aes(label = Coefficient), color = "white", size = 5)  +  # Add coefficient values
  labs(
    title = "Regression Coefficients Matrix",
    x = "Predictor Variables",
    y = "Outcome Variables"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )

