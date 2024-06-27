yt <- read.csv("C:/Users/Temidayo/Desktop/Youtube_dataset_all_dataScience_channels.csv")
yt

head(yt)
tail(yt)
data.class(yt$Like_count)
data.class(yt$Channel_Name)
data.class(yt$Title)
data.class(yt$Published_date)
data.class(yt$Views)
data.class(yt$Comment_Count)
summary(yt)

library(tidyverse)
library(learnr)
install.packages("DataExplorer")
library(DataExplorer)
library(explore)
library(SmartEDA)
library(wordcloud2)
library(readxl)
library(DataExplorer)
library(tidytext)
library(wordcloud)
library(topicmodels)
library(tm)
library(visNetwork)
library(qdapRegex)
library(syuzhet)
library(ggraph)


yt %>% plot_intro()
yt %>% plot_missing()
yt %>% introduce()
yt %>% plot_intro()
yt %>% profile_missing()
yt %>% plot_density()
yt %>% plot_histogram()
yt %>% plot_bar()
yt %>% plot_correlation(maxcat = 28)
yt %>% explore()
yt %>% select(-Title) %>% explore()
yt %>% describe_all()
yt %>% describe_cat(Views)
str(yt)
data.frame(yt)
max(yt$Views)

# cleaning the dataset of all NA
clean_yt <- na.omit(yt)
print(clean_yt)

# names of the columns
names(clean_yt)

# the highest views
max(clean_yt$Views)

# highest Like_count
max(clean_yt$Like_count)

# highest Comment_Count
max(clean_yt$Comment_Count)

# names of all column
names(clean_yt)

# total view across all the channel
sum(clean_yt$Views)

# total comment count across all the channel
sum(clean_yt$Comment_Count)

# total like count across all the channel
sum(clean_yt$Like_count)

# names of all the channel
unique(clean_yt$Channel_Name)

# publish dates for all videos 
unique(clean_yt$Published_date)

# title of all videos
unique(clean_yt$Title)

# video with the hihest views
max(clean_yt$Views)
highest_views <- which(clean_yt == "44191019", arr.ind = TRUE)
highest_views
row_highest_views <- clean_yt[20888, ]
row_highest_views

# smallest view, like_count & comment_count
min(clean_yt$Views)
min(clean_yt$Like_count)
min(clean_yt$Comment_Count)


# list of channels with zero views
lowest_view <- which(clean_yt == "0", arr.ind = TRUE)
lowest_view


# first/oldest video published 
yt_date <- yt$Published_date
yt_date
first_vid <- min(yt_date)
first_vid
first_v <- which(clean_yt == "2008-02-29", arr.ind = TRUE)
first_v
video_first <- clean_yt[8392, ]
video_first


# the latest video published
last_video <- max(yt_date)
last_video

video_last <- which(clean_yt == "2024-06-12", arr.ind = TRUE)
video_last
late_video1 <- clean_yt[1586, ]
late_video1

late_video2 <- clean_yt[3638, ]
late_video2

late_video3 <- clean_yt[17519, ]
late_video3

late_video4 <- clean_yt[19793, ]
late_video4

late_video5 <- clean_yt[21462, ]
late_video5



# channel with the least video
min_chan <- clean_yt$Channel_Name
min_chan
chan_min <- min(min_chan)
chan_min


# channel with the most videos
max_chan <- clean_yt$Channel_Name
max_chan
chan_max <- max(max_chan)
chan_max


# the number of videos from the highest and lowest channel
sum(clean_yt$Channel_Name == "Alex The Analyst")
sum(clean_yt$Channel_Name == "WsCube Tech")

names(clean_yt)


# to get the most discussed topic using word cloud 
talked <- clean_yt$Title
talked
convert_talk <- Corpus(VectorSource(talked))
convert_talk
talked_processed <- sapply(convert_talk, as.character)
talked_processed
comb_talked <- paste(talked_processed, collapse = " ")
comb_talked
split_words <- unlist(strsplit(comb_talked, "\\s+"))
split_words
word_freq <- table(split_words)
word_freq
wordcloud(words = names(word_freq), freq = word_freq, min.freq = 5,
          max.words = 200, random.order = FALSE, rot.per = 0.3, colors = custom_colors)

names(clean_yt)
install.packages("package_name")
library(package_name)


freq <- clean_yt(
  Title = c ("Python", "Probability", "Excel", "Machine Learing", "SQL", "Analysis", "Java", "Conditional Statement", "HTML", "Function", "VLOOKUP", "Tableau", "Regression")
)

freq_count <- table(freq$Title)

most_discussed <- names(freq_count)[which.max(freq_count)]

print(most_discussed)