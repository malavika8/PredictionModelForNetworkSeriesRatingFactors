# PredictionModelForNetworkSeriesRatingFactors
Project Scope
In the midst of the streaming wars, network television is finding the lifespan of it's shows extended indefinitely. The best example of this is The Office, which has led all other content in streams on Netflix over the past few years. With this in mind, the team will be conducting an analysis on which components of an episode have the largest effect on its IMDb rating. At the conclusion of the analysis, the team hopes to be able to pinpoint what aspects of a television episode led to a higher rating. 
Data Cleaning
●	Search and Elimination of Outliers in IMDb ratings
●	Scaling Number of Voters
●	One-Hot-Encoding for Writers and Directors
Model Creation
●	Identified 132 indicator variables across directors, writers, characters
●	Identified 4 continuous variables with sentiment analysis and imdb participation
●	Ran through 72 different training iterations of a neural net
●	Reviewed residuals and training results to select best possible parameters
●	Used MSE, RMSE, and MAPE scores for statistical comparison

Conclusions:
Episode ratings seem to be mostly story driven, as poor/strong writers and directors have heightened impacts on results
Sentiment, emotion, and profanity have little to no impact on IMDb ratings
Characters have -.0157 of an impact on ratings (essentially 0)
Of all 4 categories, writers had the most impact on the imdb rating.

