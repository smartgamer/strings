
# https://www.r-bloggers.com/manipulating-strings-with-the-stringr-package/

# the most common string operations: detecting, locating, matching, searching and replacing, and exctracting/removing strings.
library(tidyverse)

winchester <- read_lines("https://gist.githubusercontent.com/b-rodrigues/5139560e7d0f2ecebe5da1df3629e015/raw/e3031d894ffb97217ddbad1ade1b307c9937d2c8/gistfile1.txt")
str(winchester)
winchester[2]

# Detecting, getting the position and locating strings ----

winchester %>%
  str_detect("CONTENT")

winchester %>%
  str_which("CONTENT")

#  If we need more precision, we can use str_locate() and str_locate_all(). To explain how both these functions work, let’s create a very small example:

ancient_philosophers <- c("aristotle", "plato", "epictetus", "seneca the younger", "epicurus", "marcus aurelius")

# Now suppose I am interested in philosophers whose name ends in us. Let us use str_locate() first:

ancient_philosophers %>%
  str_locate("us")

# To get both us strings, you need to use str_locate_all():

ancient_philosophers %>%
  str_locate_all("us")


winchester[43]

# Splitting strings  ----

# There are two functions included in {stringr} to split strings, str_split() and str_split_fixed(). 
# If we want to split their names at the space character, we can use str_split() like this:

ancient_philosophers %>%
  str_split(" ")

# str_split() also has a simplify = TRUE option:

ancient_philosophers %>%
  str_split(" ", simplify = TRUE)

# This time, the returned object is a matrix.

# What about str_split_fixed()? The difference is that here you can specify the number of pieces to return. For example, you could consider the name “Aurelius” to be the middle name of Marcus Aurelius, and the “the younger” to be the middle name of Seneca the younger. This means that you would want to split the name only at the first space character, and not at all of them. This is easily achieved with str_split_fixed():

ancient_philosophers %>%
  str_split_fixed(" ", 2)

winchester_text <- winchester[43] %>%
  str_split(">")
str(winchester_text)

# Since this is a list of only one element, we can simplify it by saving the atomic vector in a variable:

winchester_text <- winchester_text[[1]]

# Let’s now look at some lines:

winchester_text[1232:1245]

# We can narrow it down to the lines that only contain the string we are interested in, “CONTENT”. First, let’s get the indices:

content_winchester_index <- winchester_text %>%
  str_which("CONTENT")

# How many lines contain the string “CONTENT”?

length(content_winchester_index)

# As you can see, this reduces the amount of data we have to work with. Let us save this is a new variable:

content_winchester <- winchester_text[content_winchester_index]

# Matching strings -----

# Matching strings is useful, but only in combination with regular expressions.
ancient_philosophers %>%
  str_match("us")
ancient_philosophers %>%
  str_match(".*us")
ancient_philosophers %>%
  str_match(".us")
ancient_philosophers %>%
  str_match("..us")

#  use the *, which matches zero or more of .. So by combining * and ., we can match any symbol repeatedly, until there is nothing more to match. Note that there is also +, which works similarly to *, but it matches one or more symbols.

# There is also a str_match_all():

ancient_philosophers %>%
  str_match_all(".*us")

c("haha", "huhu") %>%
  str_match("ha")
c("haha", "huhu") %>%
  str_match_all("ha")

# match names containing the letter “t”? Easy:

ancient_philosophers %>%
  str_match(".*t.*")

# get the strings that come after “CONTENT”:

winchester_content <- winchester_text %>%
  str_match("CONTENT.*")

# Let’s use our faithful str() function to take a look:

winchester_content %>%
  str

# Let’s us remove all these NAs. Because the result is a matrix, we cannot use the filter() function from {dplyr}. So we need to convert it to a tibble first:

winchester_content <- winchester_content %>%
  as.tibble() %>%
  filter(!is.na(V1))

# Because matrix columns do not have names, when a matrix gets converted into a tibble, the firt column gets automatically called V1. This is why I filter on this column. Let’s take a look at the data:

head(winchester_content)

# Searching and replacing strings ----
# rename the column and change all the strings to lowercase:
winchester_content <- winchester_content %>% 
  mutate(content = tolower(V1)) %>% 
  select(-V1)
head(winchester_content)

# The second part of the string, “wc=….” is not really interesting. Let’s search and replace thiswith an empty string, using str_replace():

winchester_content <- winchester_content %>% 
  mutate(content = str_replace(content, "wc.*", ""))

head(winchester_content)

winchester_content <- winchester_content %>% 
  mutate(content = str_replace(content, "content=", ""))

head(winchester_content)

# Exctracting or removing strings  -----

# Now, because I now the ALTO spec, I know how to find words that are split between two sentences:

winchester_content %>% 
  filter(str_detect(content, "hyppart"))

# For instance, the word “average” was split over two lines, the first part of the word, “aver” on thefirst line, and the second part of the word, “age”, on the second line. We want to keep what comes after “subs_content”. Let’s extract the word “average” using str_extract(). However, because only some words were split between two lines, we first need to detect where the string “hyppart1” is located, and only then can we extract what comes after “subs_content”. Thus, we need to combine str_detect() to first detect the string, and then str_extract() to extract what comes after “subs_content”:

winchester_content <- winchester_content %>% 
  mutate(content = if_else(str_detect(content, "hyppart1"), 
                           str_extract_all(content, "content=.*", simplify = TRUE), 
                           content))

# Let’s take a look at the result:

winchester_content %>% 
  filter(str_detect(content, "content"))

# We still need to get rid of the string “content=” and then of all the strings that contain “hyppart2”, which are not needed now:

winchester_content <- winchester_content %>% 
  mutate(content = str_replace(content, "content=", "")) %>% 
  mutate(content = if_else(str_detect(content, "hyppart2"), NA_character_, content))

head(winchester_content)

# to remove the " characters:
winchester_content <- winchester_content %>% 
  mutate(content = str_replace_all(content, "\"", "")) 

head(winchester_content)

# remove stop words (words that do not add any meaning to a sentence,such as “as”, “and”…) and words that are composed of less than 3 characters. You can find a dataset with stopwords inside the {stopwords} package:

library(stopwords)

data(data_stopwords_stopwordsiso)

eng_stopwords <- tibble("content" = data_stopwords_stopwordsiso$en)

winchester_content <- winchester_content %>% 
  anti_join(eng_stopwords) %>% 
  filter(nchar(content) > 3)

## Joining, by = "content"

head(winchester_content)


































































