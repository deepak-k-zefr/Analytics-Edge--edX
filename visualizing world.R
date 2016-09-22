# good and bad  ugly visuialization
intl=read.csv("intl.csv")
str(intl)
ggplot(intl,aes(x=Region,y=PercentOfIntl))+geom_bar(stat = "identity")+geom_text(aes(label=PercentOfIntl))
#stat=identity- height of bar =value of y variable
intl=transform(intl,Region=reorder(Region,-PercentOfIntl))
str(intl)
intl$PercentOfIntl=intl$PercentOfIntl*100
ggplot(intl,aes(x=Region,y=PercentOfIntl))+geom_bar(stat="identity",fill="dark blue")+geom_text(aes(label=PercentOfIntl),vjust=-0.4)+ylab("Percentage of Internationals")

intlall=read.csv("intlall.csv",stringsAsFactors = FALSE)
head(intlall)
intlall[is.na(intlall)]=0
world_map=map_data("world")
world_map=merge(world_map,intlall,by.x="region",by.y="Citizenship")
str(world_map)
ggplot(world_map,aes(x=long,y=lat,group=group))+geom_polygon(fill="white",color="black")+coord_map("mercator")
world_map=world_map[order(world_map$group,world_map$order),]
ggplot(world_map,aes(x=long,y=lat,group=group))+geom_polygon(fill="white",color="black")+coord_map("mercator")

initial