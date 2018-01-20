# Predicting the performance of kickstarter campaign: A Data Science Project

To learn more, please visit https://kickstarterforecast.wordpress.com.
To quickly learn about this project, you could refer to this [video](https://www.youtube.com/watch?v=qRkvPXF4hs8) we made.


Online crowd fundraising or crowdfunding has revolutionized the way we support creative arts and technological innovations. 
For our group project, we propose to predict the accomplishment of a crowdfunding campaign. 
It is not a simple divide between success and failure. The accomplishment of a crowdfunding campaign is measured by to what degree the actual fund exceed the expected goal.  
A project campaign that received three times more than its goal is considered more accomplished than those that just meet the line. 

We primarily investigate the following research questions:

- RQ1: How can the socio-economic factors, including the local disposable income and the currency used by the Kickstarter campaign predict the campaign performance?
- RQ2: How can the campaign properties, such as the campaign goal, reward strategy and the campaign category predict the campaign performance?
- RQ3: How can the social engagement of creators and backers, such as #updates, #comments, #Facebook friends,  predict the campaign performance?


Kickstarter dataset and city economy data were collected from [Webrobots](https://webrobots.io/kickstarter-datasets/). 

City economy data (i.e. disposible household income) is retrieved from [BEA](https://www.bea.gov/API/signup/).

We used several models for prediction, including regression, Naive Bayes, SVM, Neural Network and Clustering. 

This repository contains codes for data scraping, cleaning, processing, modeling, and finally, model comparison. 
