# Winning Percentage of College Basketball Teams Prediction

## Project Overview
The aim of this Winning Percentage Prediction project is to generate a multiple linear regression model that best predicts the winning proportion of college basketball teams based on a stat sheet of games. Regression techniques were used to build a multiple linear regression model for the dataset in order to predict the winning proportions for each team. This project is part of a private in-class Kaggle competition.

## Data
The training and testing data originally used for this project is not provided for copyright reasons, but the details are as follows:

The data is gathered from the Division I college basketball seasons in the United States for 2013-2021. Missing values were already taken care of. 

The testing data consists of 2000 observations with 20 predictor variables -- 16 numerical and 4 categorical:

| Variable Name | Description                                       | Type
|---------------|---------------------------------------------------|---------------------
|X500. Level |Whether or not a team has more wins than losses |Categorical
|ADJOE |Adjusted offensive efficiency |Numerical
|ADJDE |Adjusted defensive efficiency |Numerical
|EFG_O |Effective field goal percentage shot |Numerical
|EFG_D |Effective field goal percentage allowed |Numerical
|TOR |Turnover rate |Numerical
|TORD |Steal rate |Numerical
|ORB |Offensive rebound rate |Numerical
|DRB |Offensive rebound rate allowed |Numerical
|FTR |Free throw rate |Numerical
|FTRD |Free throw rate allowed |Numerical
|X2P_O |Two-point shooting percentage |Numerical
|X2P_D |Two-point shooting percentage allowed |Numerical
|X3P_O |Three-point shooting percentage |Numerical
|X3P_D |Three-point shooting percentage allowed |Numerical
|WAB |Wins above bubble |Numerical
|YEAR |Seasons 2013-2021 |Categorical
|NCAA |Whether team made it into the NCAA |Categorical
|Power.Rating |Level of chance of beating an average Division 1 team |Categorical
|Adjusted.Tempo |Adjusted tempo against an average Division 1 team |Numerical

## Results
Our final resulting model used 10 predictors and had an R-squared score of 0.80039, placing 5th of the 38 teams participating
