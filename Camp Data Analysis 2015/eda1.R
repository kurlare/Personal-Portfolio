## This is a script for exploratory analysis on camp data.  Make sure
## the file is in your working directory before running.

library(ggplot2)
library(dplyr)
library(mice)

campdata <- read.csv("dataset1.csv", 
                     na.strings = c("#N/A", "#DIV/0!", "#VALUE!"), 
                     header = TRUE)


## View column names and subset those behaviors selected for further analysis by
## program director.

colnames(campdata)
behav_agg <- campdata[, c(1, 2, 3, 4, 5, 23, 24, 35, 38, 39, 40, 59, 60, 61)]
dim(behav_agg)
colnames(behav_agg)
dim(behav_agg[!complete.cases(behav_agg),])  ## Number of cases with NAs.

## The number of incomplete cases is about 10% of the data.  We can proceed by 
## taking those out, or imputing some values.  Lets create an object with 
## imputed values, and we'll run analyses on both.

md.pattern(behav_agg) ## Gives us a matrix of how many rows are missing from each 
                      ## column.  Looks like we've got 69 rows missing values in 7
                      ## columns.  Removing those still leaves us with 96% of data.

## Data frame with values removed.
behav_agg.rm <- behav_agg[!is.na(behav_agg$Total.Sharing), ]  
dim(behav_agg.rm)

##  Multiple imputation with mice package.
tempData <- mice(behav_agg,m=5,maxit=10,meth='pmm',seed=500)

## Data frame with imputed values.
behav_agg.imp <- complete(tempData, 1)  
dim(behav_agg.imp)

## Add bunk variable and associated values
behav_agg.imp$bunk <- behav_agg.imp$NAME
behav_agg.imp$bunk[behav_agg.imp$bunk > 15100 & behav_agg.imp$bunk < 15200] <- 1
behav_agg.imp$bunk[behav_agg.imp$bunk > 15200 & behav_agg.imp$bunk < 15300] <- 2
behav_agg.imp$bunk[behav_agg.imp$bunk > 15300 & behav_agg.imp$bunk < 15400] <- 3
behav_agg.imp$bunk[behav_agg.imp$bunk > 15400 & behav_agg.imp$bunk < 15500] <- 4
behav_agg.imp$bunk[behav_agg.imp$bunk > 15500 & behav_agg.imp$bunk < 15600] <- 5
behav_agg.imp$bunk[behav_agg.imp$bunk > 15600 & behav_agg.imp$bunk < 15700] <- 6
behav_agg.imp$bunk[behav_agg.imp$bunk > 15700 & behav_agg.imp$bunk < 15800] <- 7

## Next step is to attemp to visualize some of the data, to gain some perspective 
## and insight on the variables.
totalpts_bunk_plot <- ggplot(data = behav_agg.imp, 
                      aes(x = Day.., y = Tot.Pos.Peer, color = factor(bunk)))

totalpts_bunk_plot + geom_point() + 
                     facet_grid(. ~ bunk) + geom_boxplot(alpha = 0.3) + 
                     labs(title = "Daily Total Points by Bunk", 
                          x = "Day", 
                          y = "Total Points Earned")

totalpts_lost_bunk_plot <- ggplot(data = behav_agg.imp[behav_agg.imp$Daily.Total.Points.Lost < 811,],
                           aes(x = Day.., y = Daily.Total.Points.Lost, color = factor(bunk)))

totalpts_lost_bunk_plot + geom_point() + 
                     facet_grid(. ~ bunk) + 
                     geom_boxplot(alpha = 0.5) + 
                     labs(title = "Daily Total Points Lost by Bunk, 90th quantile", 
                          x = "Day", 
                          y = "Total Points Lost")

## Does the average total point earned increase by day?  What about by bunk?

## Group by Day, find averages for each column, then plot and model that data.
avg_day <- behav_agg.imp %>% group_by(Day..) %>% summarise_each(funs(mean))
avg_day <- avg_day[, -c(2,3,4,5)]  ## Remove irrelevant columns

qplot(avg_day, x = avg_day$Day.., y = avg_day$Daily.Total.Points.Earned, 
      geom = "point") + geom_smooth(method = "lm")

## Regression
mean_tplmod <- lm(avg_day$Daily.Total.Points.Earned ~ avg_day$Day.., avg_day)
summary(mean_tplmod)  ## No significant relationship in daily average changes

## Group by bunk, Day, run same process.
avg_day_bunk <- behav_agg.imp %>% group_by(bunk, Day..) %>% summarise_each(funs(mean))
avg_day_bunk <- avg_day_bunk[, -c(3,4,5,6)]

qplot(avg_day_bunk, x = avg_day_bunk$Day.., 
      y = avg_day_bunk$Daily.Total.Points.Earned, 
      geom = "point")

## Does the total points earned increase over time? What about by bunk?
## Again, group by Day and bunk, visualize.  Models showed nothing signifcant 
## and are left out here.
sum_day <- behav_agg.imp %>% group_by(Day..,bunk) %>% summarise_each(funs(sum))

qplot(sum_day, x = sum_day$Day..,       ## total points per day plot by bunk
      y = sum_day$Tot.Pos.Peer, 
      geom = "path", color = factor(sum_day$bunk), size = 2) + 
      geom_point()

## Absolute Totals
sum_day2 <- behav_agg.imp %>% group_by(Day..) %>% summarise_each(funs(sum))  

qplot(sum_day2, x = sum_day2$Day.., 
      y = sum_day2$Tot.Pos.Peer,
      geom = "path", color = "red") + 
      geom_point()

## Is there a trend in the net points per day for specific children?
## Group by child, day, find averages for each child each day.
child_day <- behav_agg.imp %>% group_by(NAME, Day..) %>% summarise_each(funs(mean))

## Create net points variable by subtracting points lost from points earned.
child_day <- mutate(child_day, 
                    net.points = Daily.Total.Points.Earned - Daily.Total.Points.Lost)

## Visualize net points/day
qplot(child_day, x = child_day$Day.., y = child_day$net.points,
      geom = "point", color = factor(child_day$NAME)) + geom_path()

## Looks like there are some that have extreme negative values compared to most.
## 95% of the kids are above zero, so I'm gonna cut it off there

quantile(child_day$net.points, probs = 0.001)  ## That's extreme!
quantile(child_day$net.points, probs = 0.05) ## Find realistic quantile

above_zero <- filter(child_day, net.points > 0)
qplot(above_zero, x = above_zero$Day.., 
      y = above_zero$Tot.Pos.Peer, 
      geom = "point", color = factor(above_zero$NAME))

## Isolated this bunk because they looked to have performed the best.
## Wasn't statistically significant in itself.
bunk4.pospeer <- behav_agg.imp[behav_aggimp$bunk == 4,] 

## New data frame with extremely wild children removed.
## Director informed me that they were not representative
leave_out <- c( 15101,15103, 15104, 15108, 15111, 15112, 15204,15205, 15209, 15210,  15212,
                15301, 15307, 15308,15309, 15310, 15403, 15405,15503, 15504,15611,
                15701, 15707, 15708, 15709, 15712)

reduced <- filter(behav_agg.imp, ! NAME %in% leave_out)
