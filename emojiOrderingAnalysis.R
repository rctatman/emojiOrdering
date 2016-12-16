# analysis for relationship between argument ordering in scene discriptions in
# both text & emoji

# load in packages we'll need
library(vecsets)
library(reshape2)
library(RColorBrewer)
library(RCurl)

# this next bit will install a package to allow you to see emoji in Rstudio; it
# may not be necessary for you. Uncomment code below if it is.
# install.packages("devtools")
# install.packages("showtextdb")
# library(devtools)
# devtools::install_github("GuangchuangYu/emojifont") 
library(emojifont)
load.emojifont('OpenSansEmoji.ttf')
# list of emoji alias: http://www.webpagefx.com/tools/emoji-cheat-sheet/

# Read in file (from copy on Github)
x <- getURL("https://raw.githubusercontent.com/rctatman/emojiOrdering/master/emojiOrdering.csv")
emojiData <- read.csv(text = x, sep = ",", header = T)

# check to make sure it loaded in
head(emojiData)
summary(emojiData)

# attach data (make sure to detach!)
attach(emojiData)

# some simple dignostic plots
plot(table(CastleManEmoji, CastleManWords))
plot(table(ManMoneyEmoji, ManMoneyWords))
plot(table(ManCameraGirlEmoji, ManCameraGirlWords))

# makes tables comparing the emoji for each 
CastleMan <- as.data.frame(table(CastleManEmoji, CastleManWords))[3:10,]
ManMoney <- as.data.frame(table(ManMoneyEmoji, ManMoneyWords))[3:10,]
ManCameraGirl <- as.data.frame(table(ManCameraGirlEmoji, ManCameraGirlWords))[4:33,]

# ok, now let's do the simplest possible thing = how likely are people to
# mention the same concepts in the same order across text and emoji?
# you'll need to load the function at the end of this script for this to work
CastleMan <- cbind(CastleMan,
                   same = IfRowsMatchReturnTrue(CastleMan$CastleManEmoji, CastleMan$CastleManWords))
ManMoney <- cbind(ManMoney,
                   same = IfRowsMatchReturnTrue(ManMoney$ManMoneyEmoji, ManMoney$ManMoneyWords))
ManCameraGirl <- cbind(ManCameraGirl,
                   same = IfRowsMatchReturnTrue(ManCameraGirl$ManCameraGirlEmoji, ManCameraGirl$ManCameraGirlWords))

# so are speakers more likely to use the same or different ordering of concepts
# across emoji & words?
CastleManSameDiff <- c(sum(CastleMan$Freq[CastleMan$same == "same"]),
                       sum(CastleMan$Freq[CastleMan$same == "different"]))
ManMoneySameDiff <- c(sum(ManMoney$Freq[ManMoney$same == "same"]),
                       sum(ManMoney$Freq[ManMoney$same == "different"]))
ManCameraGirlSameDiff <- c(sum(ManCameraGirl$Freq[ManCameraGirl$same == "same"]),
                       sum(ManCameraGirl$Freq[ManCameraGirl$same == "different"]))
SameDiffAcrossPics <- cbind(ManMoneySameDiff, CastleManSameDiff, ManCameraGirlSameDiff)
rownames(SameDiffAcrossPics) <- c("same", "different")
colnames(SameDiffAcrossPics) <- c(emoji("dollar"), emoji('european_castle'), emoji('camera'))

# barplot showing people's use of same/different "syntax" across emoji &
# language
barplot(SameDiffAcrossPics, beside =T, legend.text = c("same", "different"))

# Is there a preference for an ordering that matches the scene? 

# mark each row for whether the emoji matched the orientation of the scene
CastleMan <- cbind(CastleMan,
                   orientation = IfRowsMatchReturnTrue(CastleMan$CastleManEmoji, 
                                                rep("castle man", times = length(CastleMan$CastleManEmoji))))
ManCameraGirl <- cbind(ManCameraGirl,
                   orientation = IfRowsMatchReturnTrue(ManCameraGirl$ManCameraGirlEmoji, 
                                                       rep("girl camera man", times = length(ManCameraGirl$ManCameraGirlEmoji))))
CastleManOrientation <- c(sum(CastleMan$Freq[CastleMan$orientation == "same"]),
                       sum(CastleMan$Freq[CastleMan$orientation == "different"]))
ManCameraGirlOrientation <- c(sum(ManCameraGirl$Freq[ManCameraGirl$orientation == "same"]),
                           sum(ManCameraGirl$Freq[ManCameraGirl$orientation == "different"]))
OrientationAcrossPics <- cbind(CastleManOrientation, ManCameraGirlOrientation)
rownames(OrientationAcrossPics) <- c("same", "different")
colnames(OrientationAcrossPics) <- c(emoji('european_castle'), emoji('camera'))

# plot results
barplot(OrientationAcrossPics, beside =T, legend.text = c("same", "different"),
        args.legend = list(x ="top"))

# show marginas for emoji & language ordering for just the beach photo scene:
GirlCameraManEmoji <- ManCameraGirl[ManCameraGirl$ManCameraGirlEmoji != "other",1:3]
colnames(GirlCameraManEmoji) <- c("emoji","words","freq")
levels(GirlCameraManEmoji$emoji) <- c(paste(emoji('dancer'),emoji('camera'),emoji('man')),
                                      paste(emoji('man'),emoji('camera'),emoji('dancer')),
                                      "other")
GirlCameraManEmoji <- acast(GirlCameraManEmoji, emoji~words, sum)
GirlCameraManEmoji <- t(GirlCameraManEmoji)

# significane testing
chisq.test(GirlCameraManEmoji)
# wilcoxan signed rank test
wilcox.test(GirlCameraManEmoji[,1], GirlCameraManEmoji[,2], paired = F)

# changing "girl" to "woman" to be more parallel to "man"
rownames(GirlCameraManEmoji) <- gsub("girl", "woman", rownames(GirlCameraManEmoji))

# only plot lexical orders more than 10 people (roughtly 10% of participants) used
GirlCameraManEmojiForPlot <- GirlCameraManEmoji[apply(GirlCameraManEmoji, 1, sum) > 10,]
barplot(GirlCameraManEmojiForPlot,
        legend.text = row.names(GirlCameraManEmojiForPlot), 
        beside = T,
        args.legend=list(
          x="topright",
          bty = "n" ),
        col = brewer.pal(n = length(row.names(GirlCameraManEmojiForPlot)), name = "BrBG"),
        ylim = c(0,100),
        main = "Emoji Order vs. Word Order")
# line showing chance, uncomment if you'd like to see it
# abline(h=sum(GirlCameraManEmoji/20))
# text(sum(GirlCameraManEmoji/20) + 0.5, "Chance", adj = c(-2, 0))

# simplified square showing emoji & language ordering for just the man + money scene:
ManMoneyEmoji <- ManMoney[,1:3]
colnames(ManMoneyEmoji) <- c("emoji","words","freq")
levels(ManMoneyEmoji$emoji) <- c(paste(emoji('man'),emoji('dollar')),
                                      paste(emoji('dollar'),emoji('man')))
ManMoneyEmoji <- acast(ManMoneyEmoji, emoji~words, sum)
ManMoneyEmoji <- t(ManMoneyEmoji) # transpose
# switch colummns (makes graphs nicer)
ManMoneyEmoji <- ManMoneyEmoji[,rev(colnames(ManMoneyEmoji))] 

# signifiance testing
chisq.test(ManMoneyEmoji)
# wilcoxan signed rank test
wilcox.test(ManMoneyEmoji[,1], ManMoneyEmoji[,2], paired = F)

# plot
barplot(ManMoneyEmoji, 
        legend.text = row.names(ManMoneyEmoji), 
        beside = T,
        args.legend=list(
          x="topright",
          bty = "n" ),
        ylim = c(0,100),
        col = brewer.pal(4, name = "Greys"),
        main = "Emoji Order vs. Word Order")

# line showing chance
# abline(h=sum(ManMoneyEmoji/8))
# text(sum(ManMoneyEmoji/8) + 0.5, "Chance", adj = c(-2, 0))


# simplified square showing emoji & language ordering for just the man + castle scene:
CastleManEmoji <- CastleMan[,1:3]
colnames(CastleManEmoji) <- c("emoji","words","freq")
levels(CastleManEmoji$emoji) <- c(paste(emoji('european_castle'),emoji('man')),
                                 paste(emoji('man'),emoji('european_castle')))
CastleManEmoji <- acast(CastleManEmoji, emoji~words, sum)
CastleManEmoji <- t(CastleManEmoji)

# signifiance testing
chisq.test(CastleManEmoji)
# wilcoxan signed rank test
wilcox.test(CastleManEmoji[,1], CastleManEmoji[,2], paired = F)
# so for all three picture, there's a signifiant difference overall, but different
# resposnes to the first qeustion doesn't reliably result in re-ranking of the
# possible linguisitc representations

barplot(CastleManEmoji, 
        legend.text = row.names(CastleManEmoji), 
        beside = T,
        args.legend=list(
          x="topright",
          bty = "n" 
        ),
        ylim = c(0,100),
        main = "Emoji Order vs. Word Order",
        col = brewer.pal(4, name = "YlOrBr"))
# line showing chance
# abline(h=sum(CastleManEmoji/8))
# text(sum(CastleManEmoji/8) + 0.5, "Chance", adj = c(-2, 0))

# detatch & tidy up that workspace
detach(emojiData)

# bar plots of all three pictures together
par(mfrow = c(1,3))
barplot(GirlCameraManEmojiForPlot,
        legend.text = row.names(GirlCameraManEmojiForPlot), 
        beside = T,
        args.legend=list(
          x = "topleft",
          bty = "n" ),
        col = brewer.pal(n = length(row.names(GirlCameraManEmojiForPlot)), name = "BrBG"),
        ylim = c(0,100),
        main = "Emoji Order vs. Word Order")
barplot(CastleManEmoji, 
        legend.text = row.names(CastleManEmoji), 
        beside = T,
        args.legend=list(
          x = "topleft",
          bty = "n" 
        ),
        ylim = c(0,100),
        main = "Emoji Order vs. Word Order",
        col = brewer.pal(4, name = "YlOrBr"))
barplot(ManMoneyEmoji, 
        legend.text = row.names(ManMoneyEmoji), 
        beside = T,
        args.legend=list(
          x = "topleft",
          bty = "n" ),
        ylim = c(0,100),
        col = brewer.pal(4, name = "Greys"),
        main = "Emoji Order vs. Word Order")
par(mfrow = c(1,1))

# barplots as percents, with axes labels
par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
barplot(prop.table(GirlCameraManEmojiForPlot) * 100,
        legend.text = row.names(GirlCameraManEmojiForPlot), 
        ylab = "% of responses",
        xlab = "Emoji Order",
        args.legend=list(
          x = "topleft",
          bty = "n",
          title = "Word Order"),
        col = brewer.pal(n = length(row.names(GirlCameraManEmojiForPlot)), name = "BrBG"),
        ylim = c(0,100))
barplot(prop.table(CastleManEmoji) * 100, 
        legend.text = row.names(CastleManEmoji), 
        ylab = "% of responses",
        xlab = "Emoji Order",
        args.legend=list(
          x = "topright",
          bty = "n" ,
          title = "Word Order"),
        ylim = c(0,100),
        col = brewer.pal(4, name = "YlOrBr"))
barplot(prop.table(ManMoneyEmoji) * 100, 
        legend.text = row.names(ManMoneyEmoji), 
        ylab = "% of responses",
        xlab = "Emoji Order",
        args.legend=list(
          x = "topleft",
          bty = "n" ,
          title = "Word Order"),
        ylim = c(0,100),
        col = brewer.pal(4, name = "Greys"))

# ok, so it looks like the output (emoji ordering) is affected by three
# different inputs: 1) whether there's an agent/patient relationhsip (in which
# case there's a preference for the agentive emoji to go first, possibly
# mediated by the speaker's native language--which I don't have data on) 2) the
# spatial orientation of the scene


##### Functions

# checks to see if the values at each index of two provided vectors of
# identifical length are equal, and returns a vector the same length as the
# input vectors saying wehther each index was equal. Returns an error if the
# provided vectors are of different lengths.
IfRowsMatchReturnTrue <- function(column1,column2){
  if(length(column1) != length(column2)) return("Error: columns must be same length")
  else
    returnVector = NULL
  for(i in 1:length(column1)){
    if(vsetequal(column1[i], column2[i])) returnVector <- c(returnVector, "same")
    else returnVector <- c(returnVector, "different")
  }
  return(returnVector)
}

# quick test to ensure fucntion works correctly
# dummy data:
ab <- c("a", "a", "b")
ba <- c("a", "b", "b")
abba <- c(ab, ba)

IfRowsMatchReturnTrue(ab, ba) # should return vector "same", "different", "same"
IfRowsMatchReturnTrue(ab, abba) # should return error message
