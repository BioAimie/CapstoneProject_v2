Milestone Report: Exploration of Data Sets
========================================================
author: Aimie Faucett
date: 13 June, 2016
transition: rotate
width: 1500
height: 900

<small>
Coursera Data Science Specialization </br>
Capstone Project </br>
Week 2 Report </br>
</small>

Basic Overview of English Data Sets
========================================================
The English data sets include corpora from blogs, twitter, and news sources. The data are loaded using the *readLines* command.

```r
en.blogs <- readLines('en_US/en_US.blogs.txt')
en.news <- readLines('en_US/en_US.news.txt')
en.tweets <- readLines('en_US/en_US.twitter.txt')
```

Meta data of each data set are as follows:

```
  DataSet   Size_MB LinesCount WordCount
1   Blogs 260.56432     899288  37334131
2    News  20.11139      77259   2643969
3 Twitter 316.03734    2360148  30373545
```

Data Processing and Cleaning
========================================================
Since the data sets are quite large, the first 1000 lines of the Blog data set are used for data exploration. Before running any analyses, each line is cleaned. Cleaning involves:

- Splitting each line into individual characters
- Converting all characters to lower case
- Only keeping characters that are alphabetical (e.g. no numbers or punctuation)
- Correcting spelling errors using the Peter Norvig algorithm

Once the data have been cleaned, the next step is to perform the exploratory analysis.

Exploratory Data Analysis
========================================================
One technique for predicting the next item in a sequence of words in natural language processing is using an n-gram model to find probabilites of sequences of words. A basic loop was used to find the unigram (n = 1), bigram (n = 2), and trigram (n = 3) of the first 1000 lines in the Blog data set. The code to do this is highlighted below:


```r
for (i in 1:nLines) { # where nLines = 1000
clean.charVec <- tolower(strsplit(en.blogs[i], '')[[1]])[grep(paste(' ',paste(letters, collapse='|'),sep='|'),    gsub('\\.|\\?|\\!|\\(|\\)','', tolower(strsplit(en.blogs[i], '')[[1]])))]
clean.chunk <- strsplit(paste(clean.charVec, collapse=''), ' ')[[1]]
clean.chunk <- sapply(1:length(clean.chunk), function(x) correct(clean.chunk[x]))
if(length(clean.chunk < 3)) { next() } else { # perform n-gram analysis
  gram.1 <- with(data.frame(Combo = clean.chunk, Count = 1), aggregate(Count~Combo, FUN=sum))
  gram.2 <- with(data.frame(Combo = do.call(rbind, lapply(2:length(clean.chunk), function(x) paste(clean.chunk[(x-1):x], collapse=' '))), Count = 1), aggregate(Count~Combo, FUN=sum))
  gram.3 <- with(data.frame(Combo = do.call(rbind, lapply(3:length(clean.chunk), function(x) paste(clean.chunk[(x-2):x], collapse=' '))), Count = 1), aggregate(Count~Combo, FUN=sum))
  gram.1.all <- rbind(gram.1.all, gram.1); gram.2.all <- rbind(gram.2.all, gram.2); gram.3.all <- rbind(gram.3.all, gram.3)}}
```

Results of n-gram Analysis
========================================================
At this time, the probabilites of each word given a unigram, bigram, and trigram model are not computed. Future work may involve determining these probabilies; however, results of the frequencies of each combination have been found and are displayed on the next two slides *(note that the y-axis scale is logrithmic)*. 

![plot of chunk unnamed-chunk-4](MilestoneReport_Week2-figure/unnamed-chunk-4-1.png)![plot of chunk unnamed-chunk-4](MilestoneReport_Week2-figure/unnamed-chunk-4-2.png)

Results of n-gram Analysis, continued
========================================================
Observations of interest are outlined in bullet points.

![plot of chunk unnamed-chunk-5](MilestoneReport_Week2-figure/unnamed-chunk-5-1.png)
***
- The n-1 (unigram) model looks as expected - most words appear infrequently with a small number of words appearing most often. It is expected that a small number of words like 'the', 'an', and 'I' etc. would appear very frequently.
- The n-2 (bigram) model is also expected, since it shows that words typically appear in consistent patterns. This is expected, because otherwise n-gram modeling would not be useful for natural language processing predictive algorithms. 
- The n-3 (trigram) model is less expected, as the grouping of the histogram does not appear to follow a pattern. It may be better to use the lower order models or increase beyond 3-grams to see if the distribution makes more sense.

Results of n-gram Analysis, continued
========================================================
Based on the observations on the prior slide, further analysis is done to see what the top 100 combinations (based on frequency) are using each of the n-gram models. Results are summarized in Pareto-style charts made using the *ggplot2* package. The plots axes can be difficult to read, so the charts are spread over three slides to facilitate reading.

Unigram Results:

![plot of chunk unnamed-chunk-6](MilestoneReport_Week2-figure/unnamed-chunk-6-1.png)

Results of n-gram Analysis, continued
========================================================
Bigram Results:

![plot of chunk unnamed-chunk-7](MilestoneReport_Week2-figure/unnamed-chunk-7-1.png)

Results of n-gram Analysis, continued
========================================================
Trigram Results:

![plot of chunk unnamed-chunk-8](MilestoneReport_Week2-figure/unnamed-chunk-8-1.png)

Future Work
========================================================
Based on preliminary reading regarding natural language processing, the analysis of the corpora needs to be enhanced. 
- Additional cleaning should be done to remove any remaining special characters
- Proper nouns should be handled specially, not converted to lower case automatically
- Higher-order n-gram analysis of the corpora should be performed to evaluate whether this improves the model
- The probabilites of each n-1 model should be calculated using a Markov chain method *OR*
- Another R packages, such as *tm* should be used to perform additional processing
- Output should be used as input to a machine learning algorithm, such as decision tree or random forest
- The results should be cached somehow, so that the app can run quickly and predict in real-time

The Shiny app will be developed once the predictive algorithm is complete. The app should have, at minimum, the following features:
- A user input box where a user can input some text in the form of a sentence (or a few words)
- An output that shows the next word as predicted by the model
- Some customized formatting to enhance the user's experience
- Background information about how the model works (minimal)
- Instructions for use
- Charts showing summary stats for the corpora used to create the model (optional)
