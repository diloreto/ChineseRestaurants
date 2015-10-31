# NYC Chinese Restaurant Name Choice
# (c) 10/31/15 Tony DiLoreto
# tony.diloreto@gmail.com

# Data compliments of NYC Open Data
# https://data.cityofnewyork.us/Health/DOHMH-New-York-City-Restaurant-Inspection-Results/43nn-pn8j

# Load the working directory to reference files relatively
setwd("~/Google Drive/Projects/NYCRestaurants/ChineseRestaurants")

# load the entire restaurant database into a dataframe (~180 MB)
df <- read.csv("data.csv", header = TRUE)

# Function to test the size on disk of a variable
getObjectSize <- function (object){
  print(sort( sapply(ls(),function(x){object.size(get(x))})))
}

## View the data if necessary
# View(df)

# Have a look at the file (1 row)
head(df, 1)

# We actually only want the restaurant type, restuarant names, and zips for this analysis
keeps <- c("CUISINE.DESCRIPTION" ,"DBA", "ZIPCODE")
df <- df[,(names(df) %in% keeps)]

# There are many dupliate rows, let's just keep the uniques
df <- unique.data.frame(df)

# Now we only need the Chinese restaurant rows
# First see all the unique restaurant types
sort(unique(df$CUISINE.DESCRIPTION))

# Now only keep Chinese restaurants
df.chinese <- df[df$CUISINE.DESCRIPTION=="Chinese",]

# Total number of Chinese restaurant names in NYC:
NROW(df.chinese)
# [1] 2401

# Kill original dataframe (free memory)
df <- NULL

# Now we need to do some tokenizing & word count on restaurant names
# Borrowed from https://deltadna.com/blog/text-mining-in-r-for-term-frequency/
# Install the 'tm' package & then load it
require(tm)

# In order to do proper word count,
# we really do not need all of the names as separate line items, but instead
# as one large string. Combine them now.
# Note we are assuming that no restaurant uses the same word more than once
text.of.names <- paste(df.chinese$DBA, collapse=" ")

# Now setup a text source
text.source <- VectorSource(text.of.names)

# And corresponding corpus
corpus <- Corpus(text.source)

# We can now use tm's cleaning & mapping functions to make this faster
# Full documentation at: https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
# corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Remove the word 'restaurant' as it is redundant in this analysis
corpus <- tm_map(corpus, removeWords, "restaurant")

# Now we create the document-term matrix.
dtm <- DocumentTermMatrix(corpus)

# Rebuild as a real dataframe, sorted by the most popular terms
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE) 
df.chinese.freq <- data.frame(names=names(freq), freq=freq)  

# EDA on the resultant data.frame
head(df.chinese.freq)
summary(df.chinese.freq)

# Simple analysis
# Number of time all of the words are used in NYC Chinese restaurant names
sum(df.chinese.freq$freq)
#  6766

# Number of times words with a frequency of at least 50 show up
sum(df.chinese.freq[freq > 50, ]$freq)
# 3069

# Number of times words with a frequency of at least 75 show up
sum(df.chinese.freq[freq > 75, ]$freq)
# 2541

# Number of times words with a frequency of at least 100 show up
sum(df.chinese.freq[freq > 100, ]$freq)
# 2364

# This leads us to believe the tail is very long of word frequency
# As we chop higher and higher, we are losing less and less of total word count


# Sort the data frame to ensure we have decreasing frequencies
df.chinese.freq <- transform(df.chinese.freq, names = reorder(names, -freq))

# Start plotting the results
# Courtesy of https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
require(ggplot2)

# Only keep the words that appear in at least 75 restaurant names (assuming no name uses the same word twice)
top.cutoff <- 75
df.chinese.freq.sub <- subset(df.chinese.freq, freq>=top.cutoff)

p <- ggplot(df.chinese.freq.sub, aes(names, freq))    
p <- p + geom_bar(stat="identity", fill = rainbow(n=NROW(df.chinese.freq.sub)) )   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1, size=10), axis.text.y=element_text(size=10))
p <- p + geom_text(aes(label = freq, y = freq), size = 5) + scale_size(range=c(3,6))
p

# Format plots, condition on zipcode, and more can be done from this point

# Let's make a wordcloud of it!
# install.packages("wordcloud")
require(wordcloud)

wordcloud(
  corpus, scale = c(5,0.5), max.words = 100, random.order = FALSE, rot.per =
    0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2")
)

# Please contact me at the email address above to use this code

