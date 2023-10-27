# Yelp Dataset Analysis

## Project Overview
The purpose of this project is to train and evaluate models to predict the Elite status of Yelp reviwers. Four supervised learning algorithms were used in this project: Random Forest, LDA and QDA, logistic regression, and K-Nearest Neighbors.

## Data
The data given to us is based off of the [Yelp Open Dataset](https://www.yelp.com/dataset), but had been modified for in-class use and is therefore not included. The dataset given included 53845 observations with 18 variables, which are as follows:

| Variable Name | Description|                                     
|---------------|------------|
|X |Identifying column starting from 0
|User_id |User's unique id
|Bus_id |Buisness's unique id
|Star |Rating given in a review (scale of 1-5)
|Useful |Number of "useful" reactions on a review
|Cool |Number of "cool" reactions on a review
|Funny |Number of "funny" reactions on a review
|Review |The review text
|State |State where business is located
|City |City where business is located
|Bus_Ave_Star |Average rating of a business
|User_Review_count |Number of reviews a user has made
|User_Useful_count |Total number of "useful" review reactions
|User_Funny_count |Total number of "funny" review reactions
|User_Cool_count |Total number of "cool" review reactions
|Elite |Years a user has held "elite" status
|User_Fans |Number of fans a user has
|Users_Ave_Star |Average rating given in review

## Results
The Random Forest model achieved the highest accuracy at 0.9569, followed by the K-Nearest Neighbors at 0.8816.
