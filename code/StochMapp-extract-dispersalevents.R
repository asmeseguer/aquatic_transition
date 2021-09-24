##################### 
# This script has been created by Andrea S. Meseguer and could be used to extract the number and timing of dispersal events from the output (events.tsv) of Stochastic Mapping in HiSSE Revbayes and then plots the results.
##################### 

library(plyr)
read.table("output/hisse/stochastic-mapping/events.tsv", header = T)->tabla
head(tabla)

####################
# simplify HiSSE notation to two character states
####################

# There are 4 states in our HiSSE analysis (0=1A, 1=2A, 2=1b, 3=2B), including the observed characters (1= terrestrial, 2= aquatic) and the hidden states (A, B). Therefore, in this case, terrestrial is represented by 0, 2, aquatic by 1, 3.
# We first transform the 4 states notation of HiSSE to just two characters. 
# 0 terrestrial, 1 Aquatic 
unique(tabla$end_state)
tabla$end_state[tabla$end_state==2] <- 0
tabla$end_state[tabla$end_state==3] <- 1
unique(tabla$end_state)
tabla$start_state[tabla$start_state ==2] <- 0
tabla$start_state[tabla$start_state ==3] <- 1
unique(tabla$start_state)

####################
# extract time, type (01=from land to water, 10=from water to land, 11=from water to land to water, 00=from land to warter to ) and number of dispersal events
####################

tb<-matrix(12345,ncol=5,nrow=length(unique(tabla$node_index)))
colnames(tb)<-c("node", "start-state", "end-state","time", "transition")
for(i in 1:length(unique(tabla$node_index))) {
print(i)->tb[i,1]
new <-subset(tabla, subset = node_index == i)
# extract start estate for each node
names(sort(summary(as.factor(new$start_state)), decreasing=T)[1])-> tb[i,2]
# extract end estate for each node
names(sort(summary(as.factor(new$end_state)), decreasing=T)[1])-> tb[i,3]
# calculate mean transition time
if (length(which(new$transition_type=="anagenetic")) > length(which(new$transition_type=="no_change"))) print(round(mean(na.omit(new$transition_time)),3)) -> tb[i,4]
if (length(which(new$transition_type=="anagenetic")) < length(which(new$transition_type=="no_change"))) print("no change") -> tb[i,4]
if (length(which(new$transition_type=="anagenetic")) == length(which(new$transition_type=="no_change"))) print("no change") -> tb[i,4]
# extract transition type (01 or 10)
if (tb[i,4]=="no change") print("no change")-> tb[i,5]
if (tb[i,4]!="no change") paste(tb[i,2],tb[i,3],sep="")-> tb[i,5]
}
write.table(tb, file="dispersal-stochcharmap.txt")


####################
###  plot cumulative number of dispersal events 
####################

read.table("output/hisse/stochastic-mapping/dispersal-stochcharmap.txt", header = T, stringsAsFactors=F)->tb
tb[which(tb[,4] !="no change"),-c(1:3)]->dispersal
dispersal[,3] <- 1

##### count numer of dispersals in both directions
# 0 terrestrial, 1 Aquatic 
count(dispersal$transition)

##### cumulative number of dispersal events
dispersal[which(dispersal$transition=="01" | dispersal$transition=="11"),]->towater
dispersal[which(dispersal$transition=="10" | dispersal$transition=="00"),]->toland
toland[,-2]->toland
toland2 <- toland[order(as.numeric(toland[,1])), ]
dat<- cumsum(toland2[,2])
time<- rev(as.numeric(toland2[,1]))
towater[,-2]->towater
towater2 <- towater[order(as.numeric(towater[,1])), ]
dat2<- cumsum(towater2[,2])
time2<- rev(as.numeric(towater2[,1]))

pdf("Cumulative-number-dispersal-events.pdf")
plot(time2,dat2, xlim = rev(c(0,140)), lwd=4 , type="l" , bty="n" , ylab="Cummulative number of dispersal events", col="#00BFC4", yaxt ="n",xlab="Time")
axis(side = 2,labels = TRUE, lwd=0.5,las=2)
points(time,dat,lwd=4 , type="l" , bty="n", col="#F8766D")
colors=c("#00BFC4","#F8766D")
legend("topleft", bty="n", c("Transition to water","Transition to land"), col=colors, lty=1,lwd="3",cex=0.9)
dev.off()
