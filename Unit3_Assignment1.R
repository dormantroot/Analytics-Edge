###############################################################
### Unit 3 - Assignment 1 - Popularity of Music Records #######
###############################################################
# POPULARITY OF MUSIC RECORDS
# 
# The music industry has a well-developed market with a global annual revenue around $15 billion. The recording industry is highly competitive and is dominated by three big production companies which make up nearly 82% of the total annual album sales. 
# 
# Artists are at the core of the music industry and record labels provide them with the necessary resources to sell their music on a large scale. A record label incurs numerous costs (studio recording, marketing, distribution, and touring) in exchange for a percentage of the profits from album sales, singles and concert tickets.
# 
# Unfortunately, the success of an artist's release is highly uncertain: a single may be extremely popular, resulting in widespread radio play and digital downloads, while another single may turn out quite unpopular, and therefore unprofitable. 
# 
# Knowing the competitive nature of the recording industry, record labels face the fundamental decision problem of which musical releases to support to maximize their financial success. 
# 
# How can we use analytics to predict the popularity of a song? In this assignment, we challenge ourselves to predict whether a song will reach a spot in the Top 10 of the Billboard Hot 100 Chart.
# 
# Taking an analytics approach, we aim to use information about a song's properties to predict its popularity. The dataset songs.csv consists of all songs which made it to the Top 10 of the Billboard Hot 100 Chart from 1990-2010 plus a sample of additional songs that didn't make the Top 10. This data comes from three sources: Wikipedia, Billboard.com, and EchoNest.
# 
# The variables included in the dataset either describe the artist or the song, or they are associated with the following song attributes: time signature, loudness, key, pitch, tempo, and timbre.
# 
# Here's a detailed description of the variables:
#   
# year = the year the song was released
# songtitle = the title of the song
# artistname = the name of the artist of the song
# songID and artistID = identifying variables for the song and artist
# timesignature and timesignature_confidence = a variable estimating the time signature of the song, and the confidence in the estimate
# loudness = a continuous variable indicating the average amplitude of the audio in decibels
# tempo and tempo_confidence = a variable indicating the estimated beats per minute of the song, and the confidence in the estimate
# key and key_confidence = a variable with twelve levels indicating the estimated key of the song (C, C#, . . ., B), and the confidence in the estimate
#                                                                                                     energy = a variable that represents the overall acoustic energy of the song, using a mix of features such as loudness
#                                                                                                    pitch = a continuous variable that indicates the pitch of the song
#                                                                                                     timbre_0_min, timbre_0_max, timbre_1_min, timbre_1_max, . . . , timbre_11_min, and timbre_11_max = variables that indicate the minimum/maximum values over all segments for each of the twelve values in the timbre vector (resulting in 24 continuous variables)
#                                                                                                    Top10 = a binary variable indicating whether or not the song made it to the Top 10 of the Billboard Hot 100 Chart (1 if it was in the top 10, and 0 if it was not)
# 
setwd("c:/Work/Analytics-Edge/Data")

# Problem 1.1
songs = read.csv("songs.csv")
result = songs[which(songs$year == 2010),]  # songs from 2010
str(result)


# Problem 1.2
result = songs[which(songs$artistname == "Michael Jackson"),]  # songs from Michael Jackson
str(result)


# Problem 1.3
result = songs[which(songs$artistname == "Michael Jackson"),]  # billboard top 10 songs from Michael Jackson
top10 = result[which(result$Top10 == 1),]
top10[,c("songtitle", "Top10")]

# Problem 1.4
table(songs$timesignature)


# Problem 1.5
result = songs[with(songs, order(songs$tempo,decreasing=TRUE)), ]
head(result[,c("year","songtitle","tempo")],10)  #Songs with the highest tempo


# Problem 2.1
SongsTrain = subset(songs, year <= 2009,)
SongsTest = subset(songs, year == 2010,)
str(SongsTrain)
str(SongsTest)


# Problem 2.2
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]   # remove the above variables from Train
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]      # remove the above variables from Test
SongsTop10 = glm(Top10 ~ ., data=SongsTrain, family=binomial)     # build a logistic regression model to predict Top10
summary(SongsTop10)


# Problem 2.3
# If you look at the output summary(model), where model is the name of your logistic regression model, you can see that the coefficient estimates for the confidence variables (timesignature_confidence, key_confidence, and tempo_confidence) are positive. This means that higher confidence leads to a higher predicted probability of a Top 10 hit.

# Problem 2.4
# In general, if the confidence is low for the time signature, tempo, and key, then the song is more likely to be complex. What does Model 1 suggest in terms of complexity?
# Since the coefficient values for timesignature_confidence, tempo_confidence, and key_confidence are all positive, lower confidence leads to a lower predicted probability of a song being a hit. So mainstream listeners tend to prefer less complex songs.

# Problem 2.5
# loudness has positive coeffecient, hence Mainstream listeners prefer songs with heavy instrumentation
# enegery has negative coeffecient


# Problem 3.1
cor(songs$loudness, songs$energy)    # loudness and energy are highly correlated


# Problem 3.2
# Create a model without the variable loudness
SongsLog2 = glm(Top10 ~ . -loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)


# Problem 3.3
Model3 = glm(Top10 ~ . -energy, data=SongsTrain, family=binomial)
summary(Model3)


# Problem 4.1
predictTest = predict(Model3, newdata=SongsTest, type="response")
# Confusion Matrix with threshold of 0.45
confMat = table(SongsTest$Top10, predictTest >= 0.45)
# Accuracy
(confMat[1,1] + confMat[2,2])/sum(confMat)


# Problem 4.2
# Baseline model. Pick the most frequent outcome (a song is not a Top 10 hit) for all songs
baseline = table(SongsTest$Top10)
# Accuracy
baseline[1]/(baseline[1]+baseline[2])


# Problem 4.3
predictTest = predict(Model3, newdata=SongsTest, type="response")
# Confusion Matrix with threshold of 0.45
confMat = table(SongsTest$Top10, predictTest >= 0.45)
confMat


# Problem 4.4
# Sensitivity and Specificity
confMat[2,2]/as.numeric(rowSums(confMat)[2])
confMat[1,1]/as.numeric(rowSums(confMat)[1])