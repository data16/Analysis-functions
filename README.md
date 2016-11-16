Analysis functions

The following functions take a file path as input and run analysis.
The functions will install and run all the required package.
TextAnalysis is designed for .txt files.
TwitterAnalysis is designed for Twitter data in .json format, it parses the tweets into data frame.  

The function exports into the working directory the following:

*spreadsheet of the 25 most frequent words and their frequencies
*A graph word cloud of the top 150 most frequent words
*LDA key results (a spreadsheet of how frequently a word appears and the LDA topic it was assigned to)
*N-grams (most frequent n-grams and a word cloud of them)
*For Twitter data, the function outputs a time graph of when tweets were made in terms of time of day
*It also outputs a sentiment map of tweets that are geo tagging enabled plotted onto a map with 
*sentiment represented by colour of point
*A spreadsheet of text and sentiment is also outputted 

Potential errors includes the following:
*If the format is not right, it will be unable to run
*file path needs to be specified if not working directors is set
*Often on the word clouds a non-fatal error occurs as some of the words or phases may be to long to plot

Example of how to run the function:
*TextAnalysis("alldata.txt")
*TwitterAnalysis("tweets4.json")

For more explanation of the methods used for analysis, see the following blog: https://data16blog.wordpress.com/
