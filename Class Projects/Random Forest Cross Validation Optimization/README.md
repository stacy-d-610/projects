# Random Forest Cross Validation Optimization

## Project Overview
The goal of the project was to determine which of the seven tuning parameters of random forest - ntree, replace, mtry, nodesize, maxnodes, classwt, and cutoff - affect the cross validation accuracy. Two types of experimental designs were considered: a fractional factorial design and a D-optimal design.

## Data
To help find the cross validation accuracy, the auxiliary dataset "diabetes.RData" was used to train a random forest. The dataset is not provided here, but details about the dataset are as follows:

The diabetes health indicator dataset contains 25,000 responses from the CDC’s survey and is used to predict whether a person has the diabetes or not. There are 16 predictors, 3 of which are numerical and the remaining 13 being categorical.

All of the response variable and the predictors are summarized in the following table:
| Variable| Type| Description|
|---------|-----|------------|
|Diabetes (response) |categorical |0 = no diabetes; 1 = prediabetes or diabetes
|BMI |numeric |Body Mass Index
|MentHlth |numeric |days of poor mental health scale in the past 30 days
|PhysHlth |numeric |physical illness or injury days in past 30 days
|HighBP |categorical |0 = no high BP; 1 = high BP
|HighChol |categorical |0 = no high cholesterol; 1 = high cholesterol
|CholCheck |categorical |0 = no cholesterol check in 5 years; 1 = yes cholesterol check in 5 years
|Smoker |categorical |Have you smoked at least 100 cigarettes in your entire life? (0 = no; 1 = yes)
|Stroke |categorical |Have you had a stroke? (0 = no; 1 = yes)
|PhysActivity |categorical |physical activity in past 30 days - not including job (0 = no; 1 = yes)
|Fruits |categorical |Consume Fruit 1 or more times per day (0 = no; 1 = yes)
|Veggies |categorical |Consume Vegetables 1 or more times per day (0 = no; 1 = yes)
|GenHlth |categorical |Would you say that in general your health is: scale 1-5 (1 = excellent; 2 = very good; 3 = good; 4 = fair; 5 = poor)
|DiffWalk |categorical |Do you have serious difficulty walking or climbing stairs? (0 = no 1 = yes)
|Sex |categorical |(0 = female 1 = male)
|Age |categorical |13-level age categorical

## Result
We recommend using a D-optimal design as it is Resolution V, meaning there is no aliasing between the main effects and interaction terms, which the fractional factorial design lacks. The D-optimal design also showed minimal evidence of multicolinearity. The adjusted R-squared value is 0.87.
