# set working directory
setwd("/Users/faridaadamu/Documents/Friends")

# Set the path to the folder containing the HTML files
path <- "/Users/faridaadamu/Documents/Friends/ALL_Seasons"

# Create a list of the HTML files in the folder
files <- list.files(path, pattern = "\\.html$")

# Function to extract and clean the information from each file
extract_info <- function(file) {
  
  # Read in the HTML file with ISO-8859-1 encoding
  html <- read_html(file, encoding = "ISO-8859-1")
  
  # Extract the title from the HTML using CSS selectors and replace unicode characters
  title <- html_nodes(html, "head") %>% 
    html_text() %>% 
    stri_replace_all_fixed("\u0092", "'")
  
  # Extract the text from the HTML using CSS selectors and clean the unicode characters
  body <- html_nodes(html, "p") %>% 
    html_text() %>% 
    stri_replace_all_fixed("\n", "") 
  
  # Extract the 'Transcriber' from the first paragraph
  # trans_by <- sub(".*:\\s+", "", body[1])
  # trans_by  <- gsub(".*Transcribed by: ([^\n]*)\n.*", "\\1", body[1])
  trans_by <- str_extract(body[1], "(?<=Transcribed by: )[\\w\\s]+")
  written_by <- str_extract(body[1], "(?<=Written by: )[\\w\\s]+")
  
  # Extract the scenes
  scenes <- body[-c(1)]
  scenes <- gsub("[^[:alnum:][:space:]]", "", scenes)
  
  # Return a named list with the extracted information
  return(list(title = title, trans_by = trans_by, written_by = written_by, scenes = scenes))
}

setwd("/Users/faridaadamu/Documents/Friends/ALL_Seasons")

# Apply the function to each HTML file and store the results in a list
results <- purrr::map(files, extract_info)

# Convert the list to a data frame
df <- dplyr::bind_rows(results)
```

```{r, echo=FALSE, warning=FALSE, message=FALSE, results='hide'}
#Extract the characters
df$character <- sapply(strsplit(df$scenes, " "), 
                       function(x) ifelse(length(x) > 0, x[1], NA))
df$scenes <- sub("^[^\\s]+\\s", "", df$scenes)
```

```{r echo=FALSE, warning=FALSE, message=FALSE, results='hide'}
# Remove white spaces from all columns of the dataframe
#df_clean <- data.frame(lapply(df, function(x) trimws(as.character(x))))
```

```{r echo=FALSE, warning=FALSE, message=FALSE, results='hide'}
# Convert dataframe lowercase
df[c('title', 'trans_by', 'written_by', 'scenes', 'character')] <- sapply(df[c('title', 'trans_by', 'written_by', 'scenes', 'character')], function(x) tolower(x))
```

```{r echo=FALSE, warning=FALSE, message=FALSE, results='hide'}
custom_stopwords <- c("yeah", "uh", "hey", "itâs", "gonna", "donât", "iâm", "um", "im", "rachels", "dont", "scene", "youre", "youâre", "yâknow", "thatâs", "canât", "huh", "umm", "yknow", "hes", "shes", "ii")

remove_stopwords <- function(scenes, custom_stopwords) {
  words <- unlist(strsplit(scenes, "\\s+"))
  words <- words[!tolower(words) %in% c(stop_words$word, custom_stopwords)]
  cleaned_text <- paste(words, collapse = " ")
  return(cleaned_text)
}

# Apply the function to the 'scenes' column in the data frame, passing custom_stopwords as an argument
df$scenes <- sapply(df$scenes, remove_stopwords, custom_stopwords = custom_stopwords)

# Replace 'â' with 'a' in the 'scenes' column
df$scenes <- gsub("â", "a", df$scenes)

#Remove alphanumeric
df$scenes<- gsub('[[:digit:]]+', '', df$scenes)


#Character Analysis
character_count <- df %>% 
  count(character) %>% 
  #filter(n>5000) %>% 
  arrange(desc(n)) %>% 
  head(6)

library(ggplot2)
library(viridis)

# Create a heatmap with custom color palette
ggplot(character_count, aes(x = character, y = "Number of Dialogues")) +
  geom_tile(aes(fill = n), width = 0.9, height = 0.9) +
  scale_fill_gradientn(colors = c("grey", "#006666"), na.value = NA) + # Specify custom colors
  labs(title = "Most Involved Characters in the Friends Series",
       x = "The Character",
       y = NULL,
       fill = "Number of Appearances") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.text.x = element_text( vjust = 0.5, hjust = 1),
        axis.text.y = element_text( angle = 90, vjust = 0.5, hjust = 1),
        legend.title = element_text( hjust = 0.5, vjust = 0))

# Tokenize dialogues
df1 <- df %>%
  unnest_tokens(word, scenes)

# Frequency analysis for Rachel
rachel_df <- df1 %>%
  select(character, word) %>% 
  filter(character == "rachel", !word %in% c("rachel")) %>% 
  count(word, character) %>% 
  arrange(desc(n)) %>% 
  head(10)
rachel_df

# Create a bubble chart with ggplot2
ggplot(data = rachel_df, aes(x = word, y = n, size = n)) +
  geom_point(color = "#993333", fill = "#006666", alpha = 0.7, shape = 21) +   
  scale_size(range = c(5, 15)) +  
  labs(title = "Word Frequency for Rachel", x = "Word", y = "Frequency", size = "Frequency") +
  coord_flip() + 
  theme(legend.position = "None", strip.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = "dotdash"),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line()) 

# Frequency analysis for ross
ross_df<- df1 %>%
  select(character, word) %>% 
  filter(character == "ross", !word %in% c("ross")) %>% 
  count(word, character) %>% 
  arrange(desc(n)) %>% 
  head(10)
ross_df

# Create a bubble chart with ggplot2
ggplot(data = ross_df, aes(x = word, y = n, size = n)) +
  geom_point(color = "#993333", fill = "#006666", alpha = 0.7, shape = 21) +   
  scale_size(range = c(5, 15)) +  
  labs(title = "Word Frequency for Rachel", x = "Word", y = "Frequency", size = "Frequency") +
  coord_flip() + 
  theme(legend.position = "None", strip.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = "dotdash"),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line()) 

#character sentiments
library(tidyverse)
library(tidytext)

# Count the number of words associated with each sentiment in bing
# Join the sentiments dictionaries to our clean data
sentiment_df1 <- df1 %>% 
  inner_join(get_sentiments("bing"))

# Count the occurrences of each sentiment
character_sentiments <- sentiment_df1 %>% 
  count(sentiment, character) %>%  # Include both sentiment and character in count() 
  arrange(desc(n)) %>% 
  head(11)
character_sentiments

# Create a stacked bar chart
ggplot(character_sentiments, aes(x = character, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Positive and Negative Sentiments by Character",
       x = "Character", y = "Count") +
  scale_fill_manual(values = c("positive" = "#006666", "negative" = "#993333")) +
  theme_minimal() +
  theme(legend.title = element_blank())

#Most frequently appearing words
series_word_count <- df1 %>% 
  count(word) %>% 
  filter(n>100) %>% 
  arrange(desc(n)) 
#head(100)
series_word_count

# Filter out specific words from the series_word_count object
series_word_count <- series_word_count %>%
  filter(!word %in% c("monica", "phoebe", "ross", "rachel", "joey", "chandler"))

library(wordcloud)

# Create a word cloud
wordcloud(words = series_word_count$word, freq = series_word_count$n,
          max.words = 100, random.order = FALSE,
          colors = brewer.pal(15, "RdGy"), scale = c(3, 0.5),
          rot.per = 0.25, use.r.layout = FALSE)

#Series sentiments

# Count the number of words associated with each sentiment in nrc
# Join the sentiments dictionaries to our clean data
series_sentiment <- df1 %>% 
  inner_join(get_sentiments("nrc"))

# Count the occurrences of each sentiment
series_sentiment_count <- series_sentiment %>% 
  count(sentiment) %>%  # Include both sentiment and word in count() 
  arrange(desc(n)) %>% 
  head(10)
series_sentiment_count

# Define custom colors
my_colors <- c("positive" = "#006666", "negative" = "#993333","anticipation" =  "grey", "joy" = "#006666", "trust" =  "#006666", "fear" =  "#993333", "sadness" = "#993333", "surprise" = "grey", "anger" = "#993333", "disgust" = "#993333")

# Create the bar plot with custom colors
ggplot(series_sentiment_count, aes(x = reorder(sentiment, desc(n)), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE, position = "dodge") +
  scale_fill_manual(values = my_colors) +  # Use the custom color vector
  labs(title = "Most Common Sentiments in the Friends Series",
       x = "Sentiment Type", y = "Number of Sentiments") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, face = "italic"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dotdash"),
        legend.title = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line())

# Aggregate the dataframe by the title and trans_by columns
df_comb <- dplyr::group_by(df1, title, trans_by, written_by) %>% 
  dplyr::summarize(scenes = paste(word, collapse = " "))

# Save the cleaned data frame as a .csv
write.csv(df_comb, 'friends_comb.csv', row.names = FALSE)

#further manual cleaning is required with excel. 
#we do this and import the file, and create another data frame.
# set working directory
setwd("/Users/faridaadamu/Documents/Friends")
df_combined <- read.csv("friends-comb.csv")


#Episode Analysis
library(stringr)
library(knitr)
library(kableExtra)

# Count the number of words in each row of the "dialogue" column in df1
episode_word_count <- df_combined %>%
  mutate(epi_count = str_count(scenes, "\\S+")) %>% 
  select(title, epi_count) %>% 
  arrange(desc(epi_count)) %>% 
  head(10)
episode_word_count

episode_word_count_table <- kable(episode_word_count, 
                                  caption = "Episode Word Counts",
                                  format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(row = 0, bold = TRUE, color = "#993333")

episode_word_count_table

df_combined %>%
  filter(!is.na(written_by)) %>%
  count(written_by) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(written_by, desc(n)), y = n)) +
  geom_bar(stat = "identity", fill = "#006666") +
  labs(title = "Top 10 Writers on the Friends Series", x = "Author", y = "Number of Episodes Written") +
  theme (legend.position = "None", strip.background = element_blank(),
         panel.grid.major.y = element_line(color = "grey", linetype = "dotdash"), panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) +
  coord_flip()

df_combined %>%
  count(trans_by) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(trans_by, desc(n)), y = n)) +
  geom_bar(stat = "identity", fill = "#993333") +
  labs(title = "Top 10 Transcribers on the Friends Series", x = "Transcriber", y = "Number of Episodes Transcribed") +
  theme (axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 0.5), legend.position = "None", strip.background = element_blank(),
         panel.grid.major.y = element_line(color = "grey", linetype = "dotdash"), panel.background = element_blank(), plot.title = element_text(hjust = 0.5)) +
  coord_flip()



