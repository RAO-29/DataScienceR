
# tO PLOT DOC VS TOPIC graph
# 
# setwd <- ("..")
# parent <- getwd()
# setwd(parent)
# print(parent)

data1 <- read.csv(file="output/LDAGibbsPerDocumentPerTopicProbabilities.csv", header=TRUE, sep=",")

Probability_Of_Topic <- data1$X2
Document_Colletion <- data1$X

ggplot(data = data1, mapping = aes(x = Document_Colletion, y =Probability_Of_Topic)) +
  geom_line(alpha = 0.7) +
  geom_jitter(alpha = 1, color = "RED")


# tO PLOT term VS TOPIC graph
data2 <- read.csv(file="output/LDAGibbsPerTopicPerWordProbabilities.csv", header=TRUE, sep=",")

Topic<- data2$X
football <- data2$absence
ggplot(data = data2, mapping = aes(x = Topic, y =football)) +
  #geom_line(alpha = 0.7) +
  geom_jitter(alpha = 1, color = "black")+
  geom_area()