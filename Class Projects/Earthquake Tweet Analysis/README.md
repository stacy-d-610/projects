# Earthquake Tweet Analysis

## Project Overview
The goal of the project is to analyze tweets - messages in the form of text, photos, or videos from Twitter - and to identify which tweets have the most reach in order to help with earthquake relief efforts of the 2023 Turkey earthquake. Our hypothesis is that of the tweets from Turkey, the more likes and retweets (reshares on Twitter) - together referred to as visibility - you receive on a tweet, the more likely it is that frequent keywords are used in said tweet.

## Data
The dataset we used is the [Turkey Earthquake Tweets dataset by Gabriel Preda](https://www.kaggle.com/datasets/gpreda/turkey-earthquake-tweets?resource=download), found on Kaggle. The dataset contains 28,844 observations and 16 variables, with each column providing information about the Twitter user who made the tweet or the tweet itself, along with one identifying column.

|Variable |Description|
|---------|-----------|
|id |the ID number of a tweet
|user_name |the username of the account
|user_location |the location of the user
|user_description |the user's biography description
|user_created |date when the user's account was created
|user_followers |the number of accounts following a user
|user_friends |the number of accounts the user is following
|user_favorites |number of tweets a user has liked
|user_verified |if the account is verified or not
|date |date a tweet was posted
|text |the text of the tweet itself
|hashtags |hashtags used in a tweet
|source |what device a tweet was made from
|retweets |number of retweets a tweet recieved
|favorites |number of likes a tweet recieved
|is_retweet |if the tweet is a retweet or not
