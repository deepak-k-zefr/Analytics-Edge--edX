library(dplyr)# for spped
library(ggplot2) # for visualiztiuon

data <- read.csv("data.csv", stringsAsFactors = FALSE) # read data

train <- data[!is.na(data$shot_made_flag),] # all not NAs= train
test <- data[is.na(data$shot_made_flag),] # all NA s =test



##factoring training set into shots scored and missed
train$shot_made_flag <- as.factor(train$shot_made_flag)
train$shot_made_flag <- factor(train$shot_made_flag, levels = c("1", "0"))

#a plot to see accuracy by feature
pplot <- function(feat) {
  feat <- substitute(feat)
  ggplot(data = train, aes_q(x = feat)) +
    geom_bar(aes(fill = shot_made_flag), stat = "count", position = "fill") +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    ggtitle(paste("accuracy by", feat))
  
}

# a plot to see position by feature
courtplot <- function(feat) {
  feat <- substitute(feat)
  train %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_point(aes_q(color = feat), alpha = .7, size = 3) +
    ylim(c(33.7, 34.0883)) +
    scale_color_brewer(palette = "Set1") +
    theme_void() +
    ggtitle(paste(feat))
}
# size= size of points
#alpha= fade of points, high alpha=thick points small alpha=light points
#feat=shottype
#scale_color_brewer(palette = "Set9"= different sets of color patterns for shots 
# facet_grid(~ combined_shot_type)=sperate grids based on shot type
# ggtitle=shot types
# theme void= no background theme
#

courtplot("combined_shot_type") #hard to see here.
###############################################3333

ggplot() +
  geom_point(data = filter(train, combined_shot_type == "Jump Shot"),
             aes(x = lon, y = lat), color = "grey", alpha = 0.3, size = 2) +
  geom_point(data = filter(train, combined_shot_type != "Jump Shot"),
             aes(x = lon, y = lat, 
                 color = combined_shot_type), alpha = 0.7, size = 3) +
  ylim(c(33.7, 34.0883)) +
  scale_color_brewer(palette = "Set1") +
  theme_void() +
  ggtitle("Shot Types")


########################################################
ggplot() +
  geom_point(data = filter(train, combined_shot_type != "Jump Shot",
                           shot_distance < 5),
             aes(x = loc_x, y = loc_y, 
                 color = shot_made_flag),
             alpha = 0.7, size = 3) +
  scale_color_brewer(palette = "Set1") +
  geom_point(aes(x =0, y = 0), size = 5, shape = 4) +
  theme_void() +
  ggtitle("Shots from up close")



#333333333333333333333333333333333333333333333333333333
train$y_bins <- cut(train$loc_y, breaks = 5)
pplot(y_bins) + geom_bar() + ggtitle("Shot Distribution by x_bins") +
  theme(axis.text.y = element_blank())

###divide (cut) x locations(-250 to 250) into 50 intervals and look at shots made in each interval
# maximum nimber of shots from centre

##pplot for histograms of breaks previosuly specified. geom bar says bar graph ggtilte says title

pplot(x_bins) + theme(axis.text.x = element_blank())



prop.table(table(train$action_type, train$shot_made_flag),1) -> temp
as.data.frame.matrix(temp) -> temp
temp$shot <- rownames(temp)
ggplot(temp, aes(x = reorder(shot, `1`), y = 1)) +
  geom_point(aes(y = `1`), size = 3, color = " dark blue", stat = "identity") +
  coord_flip() +
  labs(y = "Accuracy", x = "", title = "Accuracy by Shot_type")






logit.fit=glm(shot_made_flag~shot_distance+lat+lon+shot_zone_range+period+season+playoffs+loc_x+loc_y+minutes_remaining+seconds_remaining+shot_type+shot_zone_area+shot_zone_basic+opponent,data=train, family="binomial")
logit.prob <-  predict(logit.fit,test, type = "response")


logit.pred[logit.prob > 0.5] <-  1 
logit.pred[logit.prob < 0.5] <-  0 

table(logit.pred)

logit.fit=glm(shot_made_flag~shot_distance+shot_zone_range+period+shot_type,data=train, family="binomial")
write.csv(logit.pred,file="predictions")



