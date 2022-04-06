# git-ur-kicks
What does it take to have a successful kickstater campaign? This project attempts to answer that question.

Data from over 200,000 kickstarter campagin run between 2014-2022 was analyzed and key factors were identified. A model was fit using logistic regression in R which predicted the original dataset at ~79% accuracy, which was used to drive a shiny dashboard prediction app. All relevant files are included in the repo. [Data Dictionary](https://docs.google.com/spreadsheets/d/10BwF4_YedJ3HgrH9OvXecRWBwUYSOvff0WvxB9svzM0/edit?usp=sharing)

The target audience for this application is primarily project creators who want to make something awesome, and backers who want the projects they like to succeed. The Kickstarter organization also has vested interest in a high overall project success rate - they only get paid if the funding goal is achieved.

This project was originally submitted on 13 February 2022.

Next steps include:
* Tune the model: After building the predictor app I noticed some odd behavior, such as predicted success stricly dropping with increasing duration from 1 day when it should be at a maximum around (but not on) 30 days. EDA helps explain the reason for this behavior but a fix has not been explored. General tuning and vetting of feature engineering/selection needs to be completed.
*  Polish the app: Format the app to be more visually pleasing, including reviewing plots. Top priorities are reducing clutter/clipping and improving readability.
*  Flush out EDA: Review EDA with respect to model tuning, update app plots and provide written observations.
