#Install necessary packages if not already installed
install.packages

#load necessary library if not loaded
library(RColorBrewer)

#create clustured diverging bar rchart of regression coefficients for all different genres
ggplot(coefficientsLong, aes(x = Predictor, y = Coefficient, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(
    title = "Regression Coefficients by Predictor and Outcome",
    x = "Predictor Variables",
    y = "Coefficient Value",
    fill = "Outcome Variable"
  ) +
  scale_fill_brewer(palette = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)  # Rotate x-axis labels for readability
  ) +
  geom_vline(xintercept = seq_along(coefficientsLong$Predictor) - 0.5, color = "black", size = 0.5)

#All Genres Data Frame:
allAllGenres <- acousticFeaturesPopularityDF %>%
  select(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence, tempo)

#Create a data frame for pop songs including Tempo
allPopDataFrame <- acousticFeaturesPopularityGenresDF %>%
  filter(map_lgl(genres, ~any(str_detect(.x, "pop")))) %>%
  select(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence, tempo)

#Create a data frame for rock songs including Tempo
allRockDataFrame <- acousticFeaturesPopularityGenresDF %>%
  filter(map_lgl(genres, ~any(str_detect(.x, "rock|mellow gold")))) %>%
  select(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence, tempo)

#Create a data frame for hip hop songs Including Tempo
allHiphopDataFrame <- acousticFeaturesPopularityGenresDF %>%
  filter(map_lgl(genres, ~any(str_detect(.x, "hip hop|rap"))))%>%
  select(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence, tempo)

#Create Data Frame for The mean of each acoustic feature for each genre
allAllGenresAcousticAverages <- allAllGenres %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Average")%>%
  mutate(Genre = "All Genres")

allPopAcousticAverages <- allPopDataFrame %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Average")%>%
  mutate(Genre = "Pop")

allRockAcousticAverages <- allRockDataFrame %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Average")%>%
  mutate(Genre = "Rock")

allHiphopAcousticAverages <- allHiphopDataFrame %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Average")%>%
  mutate(Genre = "Hip Hop")

#Combine all the new Data Frames
genreAcousticAverages <- bind_rows(
  allAllGenresAcousticAverages,
  allPopAcousticAverages,
  allRockAcousticAverages,
  allHiphopAcousticAverages
)

#Change the acoustic features to start with a captial letter
genreAcousticAverages <- genreAcousticAverages %>%
  mutate(Feature = str_to_title(Feature))

#Add units of measurement for acoustic features that are not measured using Spotifies 0-1 measurement
genreAcousticAverages$Feature <- ifelse(genreAcousticAverages$Feature == "Loudness",
                                        "Loudness (dB)",
                                        genreAcousticAverages$Feature)

genreAcousticAverages$Feature <- ifelse(genreAcousticAverages$Feature == "Tempo",
                                        "Tempo (BPM)",
                                        genreAcousticAverages$Feature)

#View the tibble in the console
print (n= 36, genreAcousticAverages)


# Create a composite plot of all the average acoustic feature values in different genres with genre legend

ggplot(genreAcousticAverages,
       aes(x = 1,
           y = Average,
           color = Genre)) +
  geom_segment(aes(x = 0.01,
                   xend = 2,
                   y = Average,
                   yend = Average),
               size = 1.25) +  # wide horizontal line
  facet_wrap(~Feature,
             scales = "free_y",
             strip.position = "bottom") +  # facet by feature, allow each y-axis to scale individually
  scale_x_continuous(limits = c(0, 2),
                     breaks = NULL) +  # all points are at x = 1, remove x-axis ticks
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # hide x-axis text
        axis.title.x = element_blank(),  # hide x-axis label
        panel.grid.major.x = element_blank(),  # remove vertical gridlines
        panel.grid.minor.x = element_blank(),  # remove minor vertical gridlines
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +  
  labs(y = "Mean Value",
       title = "Mean Values of Acoustic Feature's of Different Genre's") +
  scale_color_manual(values = c("All Genres" = "black",
                                "Pop" = "#98FF98", 
                                "Rock" = "#FFB74D", 
                                "Hip Hop" = "#4A90E2"))

#create a data frame with song release date: 
acousticFeaturesPopularityGenresDF
colnames(acousticFeaturesPopularityGenresDF) <- str_to_title(colnames(acousticFeaturesPopularityGenresDF))

#convert the data frame into being able to be turned into a visualisation on ggplot2
timeAcousticFeature <- acousticFeaturesPopularityGenresDF %>%
  mutate(Release_date = substr(Release_date, 1, 4)) %>%
  select(Acousticness, Danceability, Energy, Instrumentalness, Liveness, Loudness, Speechiness, Valence, Tempo, Release_date) %>%
  pivot_longer(cols = -Release_date, 
               names_to = "Acoustic Feature", 
               values_to = "Value") %>%
  rename("Release Date" = Release_date) 

#View the tibble
print (timeAcousticFeature)

# Calculate the average value of each acoustic feature per release date
timeAcousticFeature <- timeAcousticFeature %>%
  group_by(`Release Date`, `Acoustic Feature`) %>%
  summarise(avg_value = mean(Value, na.rm = TRUE), .groups = 'drop')
# Calculate the mean across all dates for each acoustic feature
featureMeans <- timeAcousticFeature %>%
  group_by(`Acoustic Feature`) %>%
  summarise(global_mean = mean(avg_value, na.rm = TRUE), .groups = 'drop')
# Merge the average values with the global mean for each feature
timeAcousticFeature <- timeAcousticFeature %>%
  left_join(featureMeans, by = "Acoustic Feature") %>%
  mutate(divergence = avg_value - global_mean) # Calculate the divergence from the global mean 

#change release date variable to numeric
timeAcousticFeature$"Release Date" <- as.numeric(timeAcousticFeature$"Release Date")

#create a clustered diverging bar chart of acoustic features in all genres changing over time
ggplot(timeAcousticFeature,
       aes(x = `Release Date`,
           y = divergence,
           fill = `Acoustic Feature`)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  facet_wrap(~`Acoustic Feature`, #Label each graph with its acoustic feature
             scales = "free_y",
             strip.position = "left") +  # Move facet labels to the left
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "purple") +  # Line at y=0 (global mean)
  scale_fill_viridis_d() +
  scale_x_continuous(breaks = seq(min(timeAcousticFeature$`Release Date`), 
                                  max(timeAcousticFeature$`Release Date`),
                                  by = 10)) +  # Set breaks every 10 years
  theme_minimal() +
  labs(x = "Release Date",
       y = "Divergence from Global Mean",
       title = "Clustered Diverging Bar Chart of Acoustic Features Across Time") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1), 
        strip.text.y = element_text(angle = 0))  # Keep y-axis labels readable

# Calculate the standard deviation of the divergence for each acoustic feature
timeAcousticFeature <- timeAcousticFeature %>%
  group_by(`Acoustic Feature`) %>%
  mutate(sdDivergence = sd(divergence, na.rm = TRUE)) %>%
  ungroup()

# Define a threshold for outliers (e.g., beyond 2 standard deviations)
threshold <- 2

# Filter out outliers
timeAcousticFeatureFiltered <- timeAcousticFeature %>%
  filter(abs(divergence) <= threshold * sdDivergence)

ggplot(timeAcousticFeatureFiltered,
       aes(x = `Release Date`,
           y = divergence)) +
  geom_bar(stat = "identity",
           position = "dodge",
           fill = "#98FF98",
           color = "#98dd98") +  # Set a fixed color for all bars
  facet_wrap(~`Acoustic Feature`,
             scales = "free_y",
             strip.position = "left") +  # Move facet labels to the left
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "purple") +  # Line at y=0 (global mean)
  scale_x_continuous(breaks = seq(1960,
                                  2020,
                                  by = 10),
                     limits = c(1960, 2020)) +  # Set x-axis range from 1960 to 2020
  theme_minimal() + #Change visualisation theme to minimal
  labs(x = "Release Date",
       y = "Divergence from Global Mean",
       title = "Clustered Diverging Bar Chart of Acoustic Features across time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.text.y = element_text(angle = 0), 
        strip.placement = "outside",
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA,))  # Remove the legend
 
