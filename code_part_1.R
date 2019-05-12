## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
require(tidyverse)

## ------------------------------------------------------------------------
# 1.
# define a function to read one text post from a directory
# The parameter of the function is directory, the directory where the text file is stored
read_post <- function(directory) {
  readLines(directory)
}

# test the function
cl <- read_post("messy/losangeles/_ant_apa_d_1bd-1ba-tennis-court_6738904358.txt")

# ----------------------------

# 2.
# Define a function to read all text posts from a directory into a single data frame, using my previous read_post() function and parameter being directory

read_all_posts <- function(directory) {
  filenames = list.files(directory, pattern="*.txt", full.names = TRUE) # get the list of text files from the directory
  posts = lapply(X = filenames, FUN = read_post) # a list of all the posts read from the directory
  split = str_split_fixed(posts, "QR|Date Posted: |Price: |Latitude: |Longitude: ", 6)
  data_frame = data.frame(split)
  names(data_frame) <- c("Title", "Text", "Date Posted", "Price", "Latitude", "Longitude") # split into 6 columns
  price = substr(split[,1], start = 1, stop = 8)
  title_price = substr(price, start = 5, stop = 8)
  data_frame$title_price = as.numeric(gsub("\\D+", "", title_price)) # extract price from the title and define it as a new column in the data frame
  data_frame$Price = as.numeric(gsub("\\D+", "", data_frame$Price))
  data_frame # return the final data frame
}

all <- read_all_posts("messy/losangeles")


## ---- include = FALSE----------------------------------------------------
# 4. 
# rental price from the title is stored in the "title_price" column
prices <- all %>%
  select(Price, title_price) 

head(prices)

price_match <- all$Price == all$title_price
sum(price_match, na.rm = TRUE) # number of TRUE values where price matches the title price
nrow(all) - sum(price_match, na.rm = TRUE) # number of FALSE values

which(all$Price != all$title_price) # which rows don't have matching prices


## ------------------------------------------------------------------------
# 5. 
# Extract deposit amount from text of the posting

result = str_match(desc$remain, "Prerequisite\\(s\\): ([a-zA-Z ]+)\\.")


deposit1 <- str_extract(all$Text, "(?<=Deposit: )[^ ]*(?= )")

deposit1 <- str_extrat(all$Text, "Deposit: ([^.]+)\\.") # match where Deposit: and (any characters) until a literal '.'

deposit2 <- gsub("[^[:digit:]. ]", "", deposit1) # deposit amount

all$deposit <- as.numeric(deposit2) # add deposits as new column of data frame

subset <- subset(all, 
                 !(is.na(all$deposit)))

subset %>%
  ggplot(aes(x = title_price, y = deposit)) +
  geom_point() +
  geom_smooth() +
  ylim(0, 6000) +
  labs(title = "Price vs Deposit", x = "Price", y = "Deposit")

## --------

6. 

class(all$Text)
vector1 <- ifelse(grepl("no pets", all$Text, ignore.case = TRUE),"none", NA)
vector2 <- ifelse(grepl("cats", all$Text, ignore.case = TRUE), "cats", NA)
vector3 <- ifelse(grepl("dogs", all$Text, ignore.case = TRUE), "dogs", NA)

vector1[is.na(vector1)] <- vector2[is.na(vector1)]
vector3[is.na(vector3)] <-  vector1[is.na(vector3)]
vector3[is.na(vector3)] <- "both"
vector3
table(vector3)

# combine all 3 vectors by NAs, replace the remaining NA with "both" pets policy, add the final vector as a column to the all data frame 
