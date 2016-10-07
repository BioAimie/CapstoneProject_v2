CapstoneReport
========================================================
author: Aimie Faucett
date: 07 October, 2016
transition: rotate
width: 1500
height: 900

<small>
Coursera Data Science Specialization </br>
Capstone Project </br>
</small>

Project Overview
========================================================
The objective of the project is to create a corpus from a collection of twitter, news, and blog entries and use this corpus to build an algorithm that will predict the next word when someone is typing text.

I decided to create my own algorithm to create the corpus rather than using a built-in R packages, because I wanted to get experience building an algorithm and see how it would perform.

First Steps:
- Read in a 3% sample of all text in the news and blogs corpuses (larger samples took too long to load)
- Twitter is excluded because tweets frequently don't use a normal sentence structure and there are many grammatically incorrect abbreviations (e.g. "u" rather than "you")

Processing the Text Data to Create a Corpus
========================================================
- With each entry, break into sentences by finding typical punctuation that would end a complete sentence (i.e. ?, ., !, ;)
- Iterate through each sentence and:
    - Convert everything to lower case
    - Keep only alphabetical characters so that numbers and punctuation are removed
    - Count words in the sentence and only keep if there are more than one words in the sentence
    - Find swear words and replace them with "swearjar" to preserve the behavior of swearing in text
    - Collapse white spaces that are an artifact of removing numbers and punctuation, but preserve spaces between words

Next, I used the do.call function to comb through all entries in the corpus and extract one-, two-, three-, and four-grams. The gram data frames were written to .csv files so they could be used in the Shiny App.


```
      X      leadGram gram Record
1  6174    the end of  the    124
2  7313   the rest of  the    120
3  4310   at the same time    105
4  4244    at the end   of     93
5 16169 when it comes   to     92
6  9249     is one of  the     81
```


Predicting Text
========================================================
To predict the text, I wrote a function that takes in a user-entered character string and does the following:
- Removes punction, numbers, punctuations, and replaces swear words with "swearjar" to match the swear pattern in the corpus
- Checks the length of the user input and:
    - If the input is 3 or more words, the function will search for a match with the 4-gram data... if a match isn't found, the function checks 3- and 2-grams, respectively.
    - If the input is less than 3 words, it will check the largest gram for a match, but if a match isn't found, it will recursively check smaller grams for a match.
    - If no match can be found in any gram, the funciton will return a random word. I chose random words that I thought were funny and mixed in the top 100 most common words from corpus... The function uses a random number selector to choose one of these words to return as a guess.


```
[1] "The following are my random words:"
```

```
 [1] "homeboy"      "taco"         "snaggletooth" "sombrero"    
 [5] "blankie"      "kittens"      "tequila"      "moose"       
 [9] "spongebob"    "fireball"    
```

Shiny App
========================================================
I created a Shiny app styled with CSS for customization. The app is hosted at: 

![plot of chunk unnamed-chunk-3](CapstoneReport-figure/unnamed-chunk-3-1.png)
