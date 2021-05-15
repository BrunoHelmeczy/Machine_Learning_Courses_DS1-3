library(dplyr)
# Dyplr & stringr refresher ----
  # mutate()
  # select()
  # filter()
  # group_by() & summarise()
  # arrange()

# dbplyr -> dplyr for databases

data(iris)
iris

# select() -----
iris %>% select(Petal.Length, Species)

iris %>% select(1:3)
iris %>% select(-Species)

iris %>% select(starts_with("Sepal"))
  # starts_with() -> String
  # ends_with() -> String
  # contains() -> String
  # matches() -> Regex

# rename() -> can do so with select() too ----
iris %>% rename(Leaf_l = Sepal.Length,
                Leaf_W = Sepal.Width) %>% head()

iris %>% filter(Petal.Length < 1 | Petal.Width < 0.3)

# arrange() -> sort / order -----
iris %>% arrange(desc(Sepal.Length),Petal.Length)


iris %>% select(matches("Sepal"), Species) %>% 
  filter(Sepal.Length >= 6) %>% 
  arrange(desc(Sepal.Width))


# mutate() -> add new column ----

# group_by() + summarize() ----


# Joins -----
left_join()
right_join()
inner_join()
full_join()
semi_join()
anti_join()


## New stuff ----
library(tidyr)

# gather() ----
iris %>% gather(key = "measure",value = "cm", 1:4)

gatherd_iris <- iris %>% 
  mutate(id = seq(1,150)) %>% 
  gather("measure","cm", 1:4)

gatherd_iris %>% spread(measure, cm) %>% 
  select(id, Species, starts_with("Sepal"))

# library(lubridate) ----
library(lubridate)

Sys.setenv(TZ = 'Europe/Budapest')
now_a_date <- ymd("2015-03-10")

now_a_date %>% class()
mdy('3/10/15')
mdy('March 10th, 2015')
dmy('the 10th of March in 2015')

myd('Mar~15~10')
now()
today()
week(now())
yday(now())



# stringr -----
library(stringr)
str_length("bla blabla blablabla")

str_c("x", "y","z")
str_c(c("x", "y","z"), collapse = ", ")

x <- c("Apple","Banana","Pear")
str_sub("bla blabla blablabla",start =1, end = 7)

str_sub(x,1,1) 

str_to_lower(str_sub(x,1,1))
str_sub(x,1,1) <- str_to_lower(str_sub(x,1,1))


str_to_lower()
str_to_upper()
str_to_title()

str_sort(x, locale = "en")

# RegEx ----
str_view()

str_view_all()

str_view(x,"an")
str_view_all(x,"an")

str_view(x,".a.") # -> . = 1 ANY character
str_view_all(x,".a.") # -> . = 1 ANY character

str_view(c("abc","a.c","bef"), "a\\.c") # match ./? -> use escape character(s) \\ 
  # 1st escape string, 2nd escape RegEx

x <- "a\\b"

str_view(x,"\\\\")

# ^ -> start of strings / $ -> end of string ----
x <- c("apple pie","apple","banana","pear")
str_view(x,"a")
str_view(x,"^a")
str_view(x,"a$")

str_view(x,"^apple$")

# character classes ----

# \d -> any digit
# \s -> any white space
# [abc] -> match a,b,c
# [^abc] -> match NOT a,b,c

str_view(c("grey","gray"), "gr(e|a)y")

# Repeated matching ----

# ? -> match 0 or 1x
# + -> match 1x or more then 1x
# * -> match 0 or more times

# {n} -> match exactly n times
# {n,} -> match n+ times
# {,m} -> match max m times
# {n,m} -> match between n & m times



str_view(x,"CC?")
str_view(x,"CC?")

str_view(fruit,"(..)\\1",match = T)
  # find group w any 2 character that repeats itself 

str_detect(x,"e")

str_detect(words,"^t") %>% sum()
str_detect(words,"^t") %>% mean()

str_detect(words,"[aeiou]$") %>% mean() # % of words ending w VOWEL

words[str_detect(words,"x$")]
str_subset(words, "x$")

str_count(words,"[aeiou]") %>% mean()

str_count(words, "[aeiou]") %>% sum()
str_count(words, "[^aeiou]") %>% sum()


sentences %>% length()
sentences %>% head()

colours <- c("red","orange","yellow","green","blue","purple")
colour_match <- str_c(colours, collapse = "|")

str_subset(sentences, colour_match)


noun <- "(a|the) ([^ ]+)"

has_noun <- sentences %>% str_subset(noun) %>% head()

has_noun %>% str_match(noun)

has_noun %>% str_extract(noun)

x

str_replace(x,"[aeiuo]","-")
str_replace_all(x,"[aeiuo]","-")

y <- c("1 house","2 cars","3 women")
str_replace_all(y, c("1" = "one", "2" = "two", "3" = "three"))

sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% head(5)


## Text from PDF / word docs / websites / images ----

# tm package 

install.packages("tm")
library(tm)

# PDFs ----
filename <- 'potato.pdf'
doc <- readPDF(control = lsit(text = "-layout"))(elem = list(uri = filename))


# Word Docs ----
install.packages("docxtractr")
library(docxtractr)

doc <- read_docx("example.docx")

docx_describe_cmnts(doc)
docx_describe_tbls(doc)

install.packages("textreadr")
library(textreadr)

doc_object %>% read_document() %>% head()
pdf_object %>% read_document() %>% head()

## HTML stuff / Websites ----
library(rvest)
t <- read_html("urlstring")
t %>% html_nodes("css","xpath") %>% html_text()

# text fomr images ----
install.packages("tesseract")
library(tesseract)
ocr("picfromurl")


#### TIDY text -----
unnest_tokens()

text_df %>% unnest_tokens(line, word)


# Workflow ----
  # Text data 
    # unnest_tokens() tidytext
  # Tidy Text
    # dplyr
  # Summarized Text
    # dplyr
  # Visualize

# Jane Austin E.g. ----
install.packages("janeaustenr")
library(janeaustenr)

original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text,regex("^chapter [\\divxlc]",
                                                ignore_case = T)))) %>% 
  ungroup()

install.packages("tidytext")
library(tidytext)

tidy_books <- original_books %>% 
  unnest_tokens(word,text)

tidy_books

data(stop_words)

tidy_books <- tidy_books %>% anti_join(stop_words)


tidy_books %>% count(word, sort = T)

install.packages("gutenbergr")
library(gutenbergr)

hgwells <- gutenberg_download(c(35,36,5230,159))
tidy_hgwells <- hgwells %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)

tidy_hgwells %>% 
  count(word, sort = T)

bronte <- gutenberg_download(c(1260,768,969,9182,767))
tidy_bronte <- bronte %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)


# Data Frame
frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austin")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  count(author,word) %>% 
  group_by(author) %>% 
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Bronte Sisters`:`H.G. Wells`)


# Correl Test -----
cor.test(data = frequency[frequency$author == "H.G. Wells",],
         ~ proportion + `Jane Austin`)

cor.test(data = frequency[frequency$author == "Bronte Sisters",],
         ~ proportion + `Jane Austin`)

# Sentiment Analysis ----
install.packages('textdata')
sentiments
library(tidytext)
library(textdata)
sentiments$sentiment %>% unique()

get_sentiments("nrc")  # Qualitative
get_sentiments('bing') # positive / negative
get_sentiments('afinn') # scored between 5 & -5

nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>% inner_join(nrcjoy)

pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

afinn <- pride_prejudice %>% inner_join(get_sentiments('afinn')) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")


