# This R program uses
# This program is based on R blogger post: https://www.r-bloggers.com/statistics-sunday-using-text-analysis-to-become-a-better-writer/
# Simple way of pulling microsoft doc off internet and starting text analysis or starting text analysis on your own writing

# Created on 2018-09-17

library(textreadr)
library(tidyverse)
library(tidytext)
library(stringr)

#here is example of pulling docx off internet from https://cran.r-project.org/web/packages/textreadr/textreadr.pdf
# url <- "https://github.com/trinker/textreadr/raw/master/inst/docs/Yasmine_Interview_Transcript.docx"
# file <- download(url)
# txt <- read_docx(file)

#for our test we'll use just a test doc in our D drive
setwd("D:/")
file<-"test.docx"
#using textreadr function to read docx file
your_text<-read_docx(file)

#creating a tibble with a line number column
John <- tibble(your_text) %>%
  mutate(linenumber = row_number())

#what is tokenizing?
#source: https://www.r-bloggers.com/statistics-sunday-tokenizing-text/
#Where it says: In text analysis, a token is any kind of meaningful text unit that can be analyzed. Frequently, a token is a word and the process of tokenizing splits up #the text into individual words and counts up how many times each word appears in the text. 
#But a token could also be a phrase (such as each two-word combination present in a text, which is called a bi-gram), a sentence, a paragraph, even a whole chapter. 
#Obviously, the size of the token you choose impacts what kind of analysis you can do. Generally, people choose smaller tokens, like words.


John_words <- John %>%
  unnest_tokens(word, your_text) %>%
  anti_join(stop_words)

#showing words used more than three times in essay
John_words %>%
  count(word, sort = TRUE) %>%
  filter(n > 3) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() + xlab(NULL) + coord_flip()

