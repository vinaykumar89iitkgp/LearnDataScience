#requires dplyr package
theurl<-"https://raw.githubusercontent.com/Mrinal4Github/LearnDataScience/master/MBA%20Starting%20Salaries%20Data.csv"
#reads it into a data frame from a csv format URL
proj2<-read.table(file=theurl,header=TRUE,",")

proj2_2=arrange(proj2,gmat_tot)#arranges the original table in ascending gmat scores
datapoints=nrow(proj2_2)#no of data points

no_of_classes=round(1+3.322*log(datapoints))#number of classes
Range=max(proj2_2[,"gmat_tot"])-min(proj2_2[,"gmat_tot"])#range of the data set
class_width=round(Range/no_of_classes)
#calls function modified_dataset1 which returns original dataset with extra column(gmatclassifier), which includes class number starting from 1
modified_dataset1=classifier_class_gmatcol_number(proj2_2,min(proj2_2[,"gmat_tot"]),max(proj2_2[,"gmat_tot"]),no_of_classes,class_width)

x=modified_dataset1$gmatclassifier
hist(x,breaks=40,col="yellow")

fd=table(modified_dataset1$gmatclassifier)
y1=cbind(fd)
y2=cbind(cumsum(y1))
y3=cbind(y2,y1)
colnames(y3)<-c(fd,cfd)
y4=y2/nrow(proj2_2)
y3=cbind(y4,y2,y1)
colnames(y3) <- c("rfd", "cfd","fd")



barplot(y3[,1],main="Barplot of RCFD",col="yellow",xlab="Classes",ylab="Normalised cumulative frequency") #rfcd barplot
barplot(y3[,2],main="Barplot of CFD",col="yellow",xlab="Classes",ylab="Cumulative frequency") #cfd barplot
barplot(y3[,3],main="Barplot of FD",col="yellow",xlab="Classes",ylab="Frequency") #fd barplot


