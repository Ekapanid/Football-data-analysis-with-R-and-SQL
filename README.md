# Football-data-analysis-with-R-and-SQL Synopsis

This is a simple database study. Specifically, we used RStudio to connect to an SQLite database which can be found here: https://www.kaggle.com/hugomathien/soccer.

After connecting to the database we extracted and loaded the different tables of the database in R. Then we manipulated the data in order to find key statistics about the players the teams and the leagues they were playing at. During this analysis we merged tables with join commands in R, used dplyr to filter and mutate data and visualized information with ggplot.

After finding these key statistics a simple RShiny app is on its way. This app will connect to the database automatically and will visualize dynamically relevant tables and plots (The app currently runs without connecting to the database but it uses the downloaded tables which were saved as datasets, each one separately).
