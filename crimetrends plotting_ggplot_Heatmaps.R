#ggplot
mvt=read.csv("mvt.csv",stringsAsFactors = FALSE)
str(mvt)
mvt$Date=strptime(mvt$Date,format="%m/%d/%y %H:%M")
mvt$weekday=weekdays(mvt$Date)
mvt$Hour=mvt$Date$hour
str(mvt)
WeekdayCounts=as.data.frame(table(mvt$weekday))
str(WeekdayCounts)
library(ggplot2)
ggplot(WeekdayCounts, aes(x=Var1,y=Freq))+geom_line(aes(group=1))
 # vari make it an ordered factor-chronological order
WeekdayCounts$Var1=factor(WeekdayCounts$Var1,ordered = TRUE,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))


ggplot(WeekdayCounts, aes(x=Var1,y=Freq))+geom_line(aes(group=1), linetype=2)+xlab("Day of the Week")+ylab("Number of motor thefts")
ggplot(WeekdayCounts, aes(x=Var1,y=Freq))+geom_line(aes(group=1), alpha=0.3)+xlab("Day of the Week")+ylab("Number of motor thefts")




###########Heatmap
#add hour of the day
table(mvt$weekday,mvt$Hour)
DayhourCounts=as.data.frame(table(mvt$weekday,mvt$Hour))
str(DayhourCounts)
DayhourCounts$Hour=as.numeric(as.character(DayhourCounts$Var2))

ggplot(DayhourCounts,aes(x=Hour,y=Freq))+geom_line(aes(group=Var1,color=Var1),size=2)
ggplot(DayhourCounts,aes(x=Hour,y=Freq))+geom_line()

DayhourCounts$Var1=factor(DayhourCounts$Var1,ordered = TRUE,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

ggplot(DayhourCounts,aes(x=Hour,y=Var1))+geom_tile(aes(fill=Freq))
# lot of crimes at midnigt
ggplot(DayhourCounts,aes(x=Hour,y=Var1))+geom_tile(aes(fill=Freq))+scale_fill_gradient(name="Total mv thefts")
+theme(axis.title.y=element_blank())


ggplot(DayhourCounts,aes(x=Hour,y=Var1))+geom_tile(aes(fill=Freq))+scale_fill_gradient(name="Total mv thefts",low = "white",high="red")

# FBI total number of muders in USA
murders=read.csv("murders.csv")
str(murders)
statesMap=map_data("state")
str(statesMap)
ggplot(statesMap,aes(x=long,y=lat,group=group))+geom_polygon(fill="white",color="black")
#Match state name variables

murders$region=tolower(murders$State)
str(murders$region)
# region in both dataframes
# has both thedat of both dataframes
murderMap=merge(statesMap,murders,by="region")
ggplot(murderMap,aes(x=long,y=lat,group=group,fill=Murders))+geom_polygon(color="black")+scale_fill_gradient(low="black",high="red",guide="legend")

ggplot(murderMap,aes(x=long,y=lat,group=group,fill=Population))+geom_polygon(color="black")+scale_fill_gradient(low="black",high="red",guide="legend")
#number of plot by 1000000 people
murderMap$MurderRate=murderMap$Murders/murderMap$Population*100000
ggplot(murderMap,aes(x=long,y=lat,group=group,fill=MurderRate))+geom_polygon(color="black")+scale_fill_gradient(low="black",high="red",guide="legend")

ggplot(murderMap,aes(x=long,y=lat,group=group,fill=MurderRate))+geom_polygon(color="black")+scale_fill_gradient(low="black",high="red",guide="legend",limits=c(0,10))
ggplot(murderMap,aes(x=long,y=lat,group=group,fill=murderMap$GunOwnership))+geom_polygon(color="black")+scale_fill_gradient(low="black",high="red",guide="legend",limits=c(0,10))
#Looking at Gun ownersship of different states
ggplot(murderMap,aes(x=long,y=lat,group=group,fill=GunOwnership*1000000))+geom_polygon(color="black")+scale_fill_gradient(low="black",high="red",guide="legend")
