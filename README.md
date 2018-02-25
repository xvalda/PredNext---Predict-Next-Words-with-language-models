Language model to predict next word in a sentence

Project summary
========================================================

The data science course from the Johns Hopkins University closes with a capstone project inspired by the Swiftkey technology: creating an app that predicts the next word in a sentence. 

This app buils on a training dataset from HC Corpora. 

Main stages of the project: 

- Pre-processing and exploring the data 
- Test and implement different language models (Stupid backoff, Katz backoff, Kneser-Ney, ...)
- Develop a shiny app where the user types some text and can click on the top 5 suggested words to complete their sentence. 

Tha app can be found here: <https://xvalda.shinyapps.io/PredNext/> 

Language Model
========================================================

We retained the stupid backoff model, here's a possible example of how it works: 
- we take the last 4 words from the input text and match them against our fivegrams training set
- if there's a match, we measure a score for each result found: count(5grams)/count(4grams)
- if none or less than 5 results are found, we "back off" to the lower order ngram: 
searching for the last 3 words of the input text in our fourgrams training set, but applying a 0.4 penalty to the scores 
... and so on. 

I documented the whole process in more details, see the references section. 


Further development
========================================================

This project could undergo some further developments:
- implementing other language models 
- optimizing the datasets
- customizing the app to each user, i.e. feeding their input text on the fly to the dataset
- dealing with long dependencies (predictions based on contextual elements and not just ngrams)
- niche usage: dataset and models adapted to specific professions: healthcare, authors, ...


Project Files and References
========================================================

<small>Project files: <https://github.com/xvalda/PredNext---Predict-Next-Words-with-language-models>

Shiny app: <https://xvalda.shinyapps.io/PredNext/>

I listed the many references I used during this project in the following document (3_PredNext_Language_Models), the most essential reference is: 
- **"Speech and Language Processing"**, Jurafsky & Martin, <https://web.stanford.edu/~jurafsky/slp3/ed3book.pdf>  
- **"Language Modeling Course Videos"**, <https://www.youtube.com/watch?v=s3kKlUBa3b0>    

You can find me on linkedin if you have any question or would like to connect: <https://www.linkedin.com/in/xavier-valdayron-9707231> </small>

