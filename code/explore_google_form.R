

#### Explore Google Form Results

# Use https://googlesheets4.tidyverse.org/ to import form
# Make simple histograms and word clouds


## Install/load packages

# install.packages("pak")
install.packages("googlesheets4")
install.packages('devtools')
require(devtools)
install_github("lchiffon/wordcloud2")
install.packages("tm")

# load packages
library(tidyverse)
library(googlesheets4)
library(wordcloud2)
library(tm)

# get data
df <- read_sheet("https://docs.google.com/spreadsheets/d/143Af3YIs6Z5V3XDSjld83p-wpJnX00tefNjEmvGuSy4/edit?resourcekey#gid=254097325") 

# clean up data (a little)
df_clean <- df %>%
  select(-c("Timestamp",
            "First and Last Name",
            "What is your Github account name?")) %>%
  rename('experience'= 'Do you have any previous experience with R, or other coding languages?') %>%
  rename('experience_level' = 'Experience Level (by Randy)') %>%
  rename('describe_experience' = 'If you do have previous coding experience, please list languages (e.g., R, Python, etc.), where you got the the experience (e.g., Statistics class, working in a lab) and how you feel about your skills.')  %>%
  mutate(experience = str_replace(experience, "  If so please describe in next question.", ""))
  

# make quick histograms

# yes/no on experience histogram using ggpl
yes_no_chart <-
  ggplot(df_clean, aes(x = experience)) +
  geom_bar() +
  labs(title = "Have you used R before?", x = "", y = "Number of People") +
  theme_minimal()

yes_no_chart


# level of experience (as assigned by Randy, 0-4 with 0 being no experience)
experience_chart <-
  ggplot(df_clean, aes(x = experience_level)) +
  geom_bar() +
  labs(title = "Level of experience (assigned by Randy, 0 =  no experinece)", x = "Level", y = "Number of people") +
  theme_minimal() 

experience_chart

# quick wordcloud

#Create a vector containing only the text
text <- df_clean$describe_experience
# Create a corpus  
docs <- Corpus(VectorSource(text))

# clean text
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# create matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df_words <- data.frame(word = names(words),freq=words)


# make word cloud
wordcloud2(data = df_words)


