

_Overview_
This project analyzes whether levels of different acoustic features of songs, can predict their popularity, and whether this changes accross different genres.

_Running the Code_
The provided R Project, provides the neccessary .csv files, and R script to run the whole project.
To run the code, open 'Individual Project Coursework.Rproj', and then select the parts of the code which you want to run within the source pane, and then click run. All visualisations that are created should be visible on the output pane, and all summaries of regressions, and statistical results should be available on the console pane.

_Analysis_
  What Analysis was run

Four multiple linear regressions are run, with eight predictors: acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, and valence. Each regression had a different outcome variable: Popularity in songs across all genres, pop songs, rock songs, and hip hop songs.

_Findings_
  Key insights from the regression analysis include:

Acousticness: Significant predictor across all models. Strongest negative relationship in Pop (B = -14.1), but a positive relationship in Hip Hop (B = 7.03).

Danceability: Significant predictor for all genres except Hip Hop. Positive relationship, strongest in Pop (B = 26.43).

Energy: Only a significant predictor for Hip Hop, with a negative relationship (B = -12.92).

Instrumentalness: Significant predictor for all except Rock. Negative relationship, strongest for all genres (B = -18.67).

Liveness: Significant predictor across all genres. Negative relationship, strongest in Rock (B = -7.28).

Loudness: Significant predictor across all genres. Positive relationship, strongest in Pop (B = 1.94).

Speechiness: Significant predictor for Pop and all genres. Strongest in all genres (B = 19.78).

Valence: Significant predictor across all genres. Negative relationship, strongest in Pop (B = -24.26).
