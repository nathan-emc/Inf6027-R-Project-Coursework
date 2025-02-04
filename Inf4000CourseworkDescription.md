Overview This project analyzes whether levels of different acoustic features of songs, can predict their popularity, and whether this changes accross different genres, focusing on visualisations.

Downloading the R Project To Download the R Project, click on <> Code on 'https://github.com/nathan-emc/Inf6027-and-Inf4000-R-Project-Coursework/'; and then download the project as a .zip file. After this, unzip the file to your desired location, and the Inf4000 R Project should be available in the folder 'Inf4000 R Project Coursework'.

Running the Code The provided R Project, provides the neccessary .csv files, and R script to run the whole project. To run the code, open 'Individual Project Coursework.Rproj', and then open the R script named 'Inf6027.R'. After this, select the parts of the code which you want to run within the source pane, and then click run. All visualisations that are created should be visible on the output pane, and all summaries of regressions, and statistical results should be available on the console pane.

After this, open an R script named 'Inf4000 Extra Graphs'. After this, repeat the process for the previous R script and additional visualisations will be created.


Analysis What Analysis was run

Four multiple linear regressions are run, with eight predictors: acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, and valence. Each regression had a different outcome variable: Popularity in songs across all genres, pop songs, rock songs, and hip hop songs.

Findings Key insights from the regression analysis include:

Acousticness: Significant predictor across all models. Strongest negative relationship in Pop (B = -14.1), but a positive relationship in Hip Hop (B = 7.03).

Danceability: Significant predictor for all genres except Hip Hop. Positive relationship, strongest in Pop (B = 26.43).

Energy: Only a significant predictor for Hip Hop, with a negative relationship (B = -12.92).

Instrumentalness: Significant predictor for all except Rock. Negative relationship, strongest for all genres (B = -18.67).

Liveness: Significant predictor across all genres. Negative relationship, strongest in Rock (B = -7.28).

Loudness: Significant predictor across all genres. Positive relationship, strongest in Pop (B = 1.94).

Speechiness: Significant predictor for Pop and all genres. Strongest in all genres (B = 19.78).

Valence: Significant predictor across all genres. Negative relationship, strongest in Pop (B = -24.26).
