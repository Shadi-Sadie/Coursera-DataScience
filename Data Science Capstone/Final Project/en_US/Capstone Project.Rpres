Capstone Project
========================================================
author: Shadi
date: 08/05/2020
autosize: true

About
========================================================

The capstone project is designed to allow students to create a usable/public data product that can be used to show skills to potential employers. The project's data is drawn from real-world data. The goal of this exercise is to create a product to highlight the prediction algorithm that can be accessed by an app interface easily used by others.

Background and Data
========================================================

Coursera provided the base corpus (text) files in order to base the language model. The corpus consisted of three files - blogs(800,000 lines), news(1,000,000 lines) and twitter(2,000,000 lines)

The training set was creating by reading from the above corpus about 500,000 lines of blogs, 600,000 lines of news and 1,000,000 lines of twitter.
The text was preprocessed by removing numbers from the text as they do not contribute to prediction capability
Other preprocessing steps like converting to lower cases, removal of punctuation etc was taken care within the tokenization process carried out by the tidy text framework and functions therein.
The text also contained a lot of hashtags, hyperlinks such as http://, www. etc. During the initial cleaning process, eliminating this from this big a corpus seemed to take a lot of time (over 2 mins) for such a small portion to be removed. Hence it was decided to keep it as such without really impacting the prediction capability.





Prediction algorithm
========================================================

The prediction model to get the next word in based on back-off method detailed as below:

Compressed data are loaded
Input a sequence of words
First, quadgram.RData is used if the last used 3 words are in the dataset
else, trigram.RData is used if the last used 2 words are in the dataset
else, bigram.RData is used if the last used word is in the dataset
else, the word with highest frequency is returned




The Application
========================================================


The user interface of the application was designed to predict English words from English text. The App has an interactive interface that refreshes the predicted word as text is being enterd.

To use the application, simply type in a word, phrase, or sentence. The app will show the next top predicted word. The user can enter additional words, or change their entry, and the app will respond to the new input.

To access The application on Shinny [application](https://shadis.shinyapps.io/Prediction/) and to access to code on [github](https://github.com/Shadi-Sedi/Capstone-Project)
