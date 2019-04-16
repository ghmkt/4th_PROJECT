library(dplyr)
library(ggplot2)
library(NbClust)
library(reshape2)
library(tidyr)

weekend_lent_tidyed <- as.tibble(read.csv("df_weekend_lent_tidyed.csv", stringsAsFactors = FALSE))
weekday_lent_tidyed <- as.tibble(read.csv("df_weekday_lent_tidyed.csv", stringsAsFactors = FALSE))
weekend_return_tidyed <- as.tibble(read.csv("df_weekend_return_tidyed.csv", stringsAsFactors = FALSE))
weekday_return_tidyed <- as.tibble(read.csv("df_weekday_return_tidyed.csv", stringsAsFactors = FALSE))

##merging data not considering month, only lent

weekend_lent_tidyed_nomonth <- (weekend_lent_tidyed %>%  group_by(lent_where) %>%  summarise_all(sum))[,-2]
weekday_lent_tidyed_nomonth <- (weekday_lent_tidyed %>%  group_by(lent_where) %>%  summarise_all(sum))[,-2]

weekend_return_tidyed_nomonth <- (weekend_return_tidyed %>%  group_by(return_where) %>%  summarise_all(sum))[,-2]
weekday_return_tidyed_nomonth <- (weekday_return_tidyed %>%  group_by(return_where) %>%  summarise_all(sum))[,-2]

head(weekend_return_tidyed_nomonth)

## Since we want to cluster these spots with lent change in time series, scale each lent counts first.

weekend_lent_tidyed_nomonth_scaled <- weekend_lent_tidyed_nomonth
weekend_lent_tidyed_nomonth_scaled[,-1] <- t(scale(t(weekend_lent_tidyed_nomonth[,-1])))

weekday_lent_tidyed_nomonth_scaled <- weekday_lent_tidyed_nomonth
weekday_lent_tidyed_nomonth_scaled[,-1] <- t(scale(t(weekday_lent_tidyed_nomonth[,-1])))

weekend_return_tidyed_nomonth_scaled <- weekend_return_tidyed_nomonth
weekend_return_tidyed_nomonth_scaled[,-1] <- t(scale(t(weekend_return_tidyed_nomonth[,-1])))

weekday_return_tidyed_nomonth_scaled <- weekday_return_tidyed_nomonth
weekday_return_tidyed_nomonth_scaled[,-1] <- t(scale(t(weekday_return_tidyed_nomonth[,-1])))


## plot before clustering, let's see the entire data
### with scaling
weekend_lent_mean_whole <- weekend_lent_tidyed_nomonth_scaled %>%  summarize_at( vars(-lent_where), mean )
weekday_lent_mean_whole <- weekday_lent_tidyed_nomonth_scaled %>%  summarize_at( vars(-lent_where), mean )

all_lent_mean_whole <- rbind(weekend_lent_mean_whole, weekday_lent_mean_whole)
all_lent_mean_whole$weekday <- c("weekend", "weekday")

ggplot(melt(all_lent_mean_whole, id.vars = "weekday"), aes(x= variable, y=value)) + #melt 써서 데이터 형식 바꿔줘야 ggplot 가능
  geom_line(aes(colour=weekday, group =weekday)) + geom_point() + ggtitle("Lent") +
theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))

weekend_return_mean_whole <- weekend_return_tidyed_nomonth_scaled %>%  summarize_at( vars(-return_where), mean )
weekday_return_mean_whole <- weekday_return_tidyed_nomonth_scaled %>%  summarize_at( vars(-return_where), mean )

all_return_mean_whole <- rbind(weekend_return_mean_whole, weekday_return_mean_whole)
all_return_mean_whole$weekday <- c("weekend", "weekday")

ggplot(melt(all_return_mean_whole, id.vars = "weekday"), aes(x= variable, y=value)) + #melt 써서 데이터 형식 바꿔줘야 ggplot 가능
  geom_line(aes(colour=weekday, group =weekday)) + geom_point() + ggtitle("Return") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))

### without scaling
weekend_lent_mean_whole_woscale <- weekend_lent_tidyed_nomonth %>%  summarize_at( vars(-lent_where), mean )
weekday_lent_mean_whole_woscale <- weekday_lent_tidyed_nomonth %>%  summarize_at( vars(-lent_where), mean )

all_lent_mean_whole_woscale <- rbind(weekend_lent_mean_whole_woscale, weekday_lent_mean_whole_woscale)
all_lent_mean_whole_woscale$weekday <- c("weekend", "weekday")

ggplot(melt(all_lent_mean_whole_woscale, id.vars = "weekday"), aes(x= variable, y=value)) + #melt 써서 데이터 형식 바꿔줘야 ggplot 가능
  geom_line(aes(colour=weekday, group =weekday)) + geom_point() + ggtitle("Lent_without_scaling") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))

weekend_return_mean_whole <- weekend_return_tidyed_nomonth_scaled %>%  summarize_at( vars(-return_where), mean )
weekday_return_mean_whole <- weekday_return_tidyed_nomonth_scaled %>%  summarize_at( vars(-return_where), mean )

all_return_mean_whole <- rbind(weekend_return_mean_whole, weekday_return_mean_whole)
all_return_mean_whole$weekday <- c("weekend", "weekday")

ggplot(melt(all_return_mean_whole, id.vars = "weekday"), aes(x= variable, y=value)) + #melt 써서 데이터 형식 바꿔줘야 ggplot 가능
  geom_line(aes(colour=weekday, group =weekday)) + geom_point() + ggtitle("Return") +
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))

write_excel_csv(all_lent_mean_whole_woscale, "lent_by_hour_mean_789.csv")





### 시간별 특성으로 클러스터한 결과

## Let's start clutering from now. We should find optimal k first.

###find optimal k in weekend data
#detect k
##detect k by barplot
weekend_lent_tidyed_nomonth_scaled[,-1] + 5

nc <- NbClust((weekend_lent_tidyed_nomonth_scaled[,-1]), min.nc=2, max.nc=10, method="kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")


## Then, execute clustering and allocate the number of cluster in df.
###WEEKEND
### kmeans in weekend

we_kmeans2 <- kmeans(weekend_lent_tidyed_nomonth_scaled[,-1], centers = 2, iter.max = 10000)
we_cluster_k2<-we_kmeans2$cluster
we_kmeans3 <- kmeans(weekend_lent_tidyed_nomonth_scaled[,-1], centers = 3, iter.max = 10000)
we_cluster_k3<-we_kmeans3$cluster
we_kmeans4 <- kmeans(weekend_lent_tidyed_nomonth_scaled[,-1], centers = 4, iter.max = 10000)
we_cluster_k4<-we_kmeans4$cluster
we_kmeans5 <- kmeans(weekend_lent_tidyed_nomonth_scaled[,-1], centers = 5, iter.max = 10000)
we_cluster_k5<-we_kmeans5$cluster

#### merge cluster_num with dataset and get means group by that cluster num

we_lent_tidyed_nomonth_scaled_cluster <- cbind(weekend_lent_tidyed_nomonth_scaled, we_cluster_k2, we_cluster_k3, we_cluster_k4, we_cluster_k5)

we_lent_mean_cluster2 <- we_lent_tidyed_nomonth_scaled_cluster[,1:26] %>% group_by(we_cluster_k2) %>% 
  summarize_at( vars(-lent_where), mean )

we_lent_mean_cluster3 <- we_lent_tidyed_nomonth_scaled_cluster[,c(1:25,27)] %>% group_by(we_cluster_k3) %>% 
summarize_at( vars(-lent_where), mean )

we_lent_mean_cluster4 <- we_lent_tidyed_nomonth_scaled_cluster[,c(1:25,28)] %>% group_by(we_cluster_k4) %>% 
  summarize_at( vars(-lent_where), mean )

we_lent_mean_cluster5 <- we_lent_tidyed_nomonth_scaled_cluster[,c(1:25,29)] %>% group_by(we_cluster_k5) %>% 
  summarize_at( vars(-lent_where), mean )


#### Then, draw ggplot

ggplot(melt(we_lent_mean_cluster2, id.vars = "we_cluster_k2"), aes(x= variable, y=value)) + #melt 써서 데이터 형식 바꿔줘야 ggplot 가능
  geom_line(aes(colour=we_cluster_k2, group = we_cluster_k2)) + geom_point()

ggplot(melt(we_lent_mean_cluster3, id.vars = "we_cluster_k3"), aes(x= variable, y=value)) + #melt 써서 데이터 형식 바꿔줘야 ggplot 가능
  geom_line(aes(colour=we_cluster_k3, group = we_cluster_k3)) + geom_point()

ggplot(melt(we_lent_mean_cluster4, id.vars = "we_cluster_k4"), aes(x= variable, y=value)) + #melt 써서 데이터 형식 바꿔줘야 ggplot 가능
  geom_line(aes(colour=we_cluster_k4, group = we_cluster_k4)) + geom_point()

ggplot(melt(we_lent_mean_cluster5, id.vars = "we_cluster_k5"), aes(x= variable, y=value)) + #melt 써서 데이터 형식 바꿔줘야 ggplot 가능
  geom_line(aes(colour=we_cluster_k5, group = we_cluster_k5)) + geom_point()


###WEEKDAY
### kmeans in weekday

wd_kmeans2 <- kmeans(weekday_lent_tidyed_nomonth_scaled[,-1], centers = 2, iter.max = 10000)
wd_cluster_k2<-wd_kmeans2$cluster
wd_kmeans3 <- kmeans(weekday_lent_tidyed_nomonth_scaled[,-1], centers = 3, iter.max = 10000)
wd_cluster_k3<-wd_kmeans3$cluster
wd_kmeans4 <- kmeans(weekday_lent_tidyed_nomonth_scaled[,-1], centers = 4, iter.max = 10000)
wd_cluster_k4<-wd_kmeans4$cluster
wd_kmeans5 <- kmeans(weekday_lent_tidyed_nomonth_scaled[,-1], centers = 5, iter.max = 10000)
wd_cluster_k5<-wd_kmeans5$cluster

#### merge cluster_num with dataset and get means group by that cluster num

wd_lent_tidyed_nomonth_scaled_cluster <- cbind(weekday_lent_tidyed_nomonth_scaled, wd_cluster_k2, wd_cluster_k3, wd_cluster_k4, wd_cluster_k5)

wd_lent_mean_cluster2 <- wd_lent_tidyed_nomonth_scaled_cluster[,1:26] %>% group_by(wd_cluster_k2) %>% 
  summarize_at( vars(-lent_where), mean )

wd_lent_mean_cluster3 <- wd_lent_tidyed_nomonth_scaled_cluster[,c(1:25,27)] %>% group_by(wd_cluster_k3) %>% 
  summarize_at( vars(-lent_where), mean )

wd_lent_mean_cluster4 <- wd_lent_tidyed_nomonth_scaled_cluster[,c(1:25,28)] %>% group_by(wd_cluster_k4) %>% 
  summarize_at( vars(-lent_where), mean )

wd_lent_mean_cluster5 <- wd_lent_tidyed_nomonth_scaled_cluster[,c(1:25,29)] %>% group_by(wd_cluster_k5) %>% 
  summarize_at( vars(-lent_where), mean )


#### Then, draw ggplot

ggplot(melt(wd_lent_mean_cluster2, id.vars = "wd_cluster_k2"), aes(x= variable, y=value)) + #melt 써서 데이터 형식 바꿔줘야 ggplot 가능
  geom_line(aes(colour=wd_cluster_k2, group = wd_cluster_k2)) + geom_point()

ggplot(melt(wd_lent_mean_cluster3, id.vars = "wd_cluster_k3"), aes(x= variable, y=value)) + #melt 써서 데이터 형식 바꿔줘야 ggplot 가능
  geom_line(aes(colour=wd_cluster_k3, group = wd_cluster_k3)) + geom_point()

ggplot(melt(wd_lent_mean_cluster4, id.vars = "wd_cluster_k4"), aes(x= variable, y=value)) + #melt 써서 데이터 형식 바꿔줘야 ggplot 가능
  geom_line(aes(colour=wd_cluster_k4, group = wd_cluster_k4)) + geom_point()

ggplot(melt(wd_lent_mean_cluster5, id.vars = "wd_cluster_k5"), aes(x= variable, y=value)) + #melt 써서 데이터 형식 바꿔줘야 ggplot 가능
  geom_line(aes(colour=wd_cluster_k5, group = wd_cluster_k5)) + geom_point()


##data for visualization
#서론, 주빈요청

df_stock_tidyedbyhour <- df_stock_tidyedbytime %>%  group_by(weekend, hour) %>% 
  summarize(under_1_sta_pro_mean_ = mean(under_1_sta_pro_mean))

write_excel_csv(df_stock_tidyedbyhour, "present_for_jubin_1ordown_mean.csv")


#본론 1, 시간대 관련

df_stock_withrest_tidyedbytime
df_stock_over90_tidyedbyhour <- df_stock_withrest_tidyedbytime %>%  group_by(weekend, hour) %>% summarize(over_90_pro_mean = mean(over_90_sta_pro_mean))

all_lent_mean_whole_woscale_ <- melt(all_lent_mean_whole_woscale)
all_lent_mean_whole_woscale_$hour <- rep(0:23, each=2)
all_lent_mean_whole_woscale_ <- all_lent_mean_whole_woscale_[,c(1,4,3)]

all_lent_mean_whole_woscale_[all_lent_mean_whole_woscale_$weekday == "weekend",1] <- TRUE
all_lent_mean_whole_woscale_[all_lent_mean_whole_woscale_$weekday == "weekday",1] <- FALSE
colnames(all_lent_mean_whole_woscale_)[1] <- "weekend"

df_stock_tidyedbyhour$hour <- as.integer(df_stock_tidyedbyhour$hour)


all_lent_mean_whole_woscale_ <- as.tibble(all_lent_mean_whole_woscale_)
colnames(all_lent_mean_whole_woscale_)[3] <- "lent_count_mean"


all_lent_mean_whole_woscale_$weekend <- as.logical(all_lent_mean_whole_woscale_$weekend)
df_stock_under1_tidyedbyhour
df_stock_over90_tidyedbyhour$hour <- as.integer(df_stock_over90_tidyedbyhour$hour)

main_1_visualization <- inner_join(all_lent_mean_whole_woscale_, df_stock_under1_tidyedbyhour)
main_1_visualization_ <- inner_join(main_1_visualization, df_stock_over90_tidyedbyhour)

write_excel_csv(main_1_visualization_, "main_1_visualization.csv")


