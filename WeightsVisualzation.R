library(ggplot2)

weightsVariable <- read.csv("Variable_Weights.csv")
str(weightsVariable)
weightsVariable$Variables <- as.character(weightsVariable$Variables)
ggplot(weightsVariable ,aes(x= Actors.Directors.Writers,y = Weights,label=Weights)) + geom_point(stat = 'identity',fill = "lightgreen",size = 6) + geom_segment(aes(y=0,x = Actors.Directors.Writers,yend = Weights,xend = Actors.Directors.Writers),color = "orange") + geom_text(color = "blue",size = 3) + ylim(-0.6,0.6) + coord_flip()

ggplot(weightsVariable ,aes(x= Actors.Directors.Writers,y = Weights,label=Weights)) + geom_point(stat = 'identity',fill = "darkgreen",size = 3) +geom_segment(aes(y=0,x = Actors.Directors.Writers,yend = Weights,xend = Actors.Directors.Writers),color = "orange",size = 1) + geom_text(vjust = -1.4,color = "darkblue",size = 2) + theme(axis.text.x = element_text(angle = 90)) + ylim(-0.6,0.6) + coord_flip()

#scale_color_manual(name = "Weights",labels = c("Positive","Negative"),values = c("above" ="#00ba38","below"="#f8766d"))