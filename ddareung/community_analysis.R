
#패키지 igraph
library(igraph)

#데이터 로드 및 병합
pub_bike_loc<- read.csv("pub_bike_list.csv",header = TRUE)
pub_bike_t1<- read.csv('data (1)/서울특별시 공공자전거 대여이력 정보_2017년_3분기_1.csv',header = TRUE,stringsAsFactors = FALSE)
pub_bike_t2<- read.csv('data (1)/서울특별시 공공자전거 대여이력 정보_2017년_3분기_2.csv',header = TRUE,stringsAsFactors = FALSE)

pub_bike_t<- rbind(pub_bike_t1,pub_bike_t2)
pub_bike_t<- as.data.frame(pub_bike_t)

#데이터 문자열 처리
pub_bike_t[,3] <- gsub("'","",pub_bike_t[,3])
pub_bike_t[,7] <- gsub("'","",pub_bike_t[,7])

#각 정류소 이름을 열 이름으로 설정하기 위해서 정류소 이름들 저장
stagelist <- unique(c(unique((pub_bike_t[,3])),unique(pub_bike_t[,7])))

#인접행렬 구서
N <- length(stagelist)
Adj <- matrix(0, N, N)

rownames(Adj) <- stagelist
colnames(Adj) <- stagelist

for(j in 1:length(pub_bike_t[,1])){
  
  Adj[unlist(pub_bike_t[j,3]),unlist(pub_bike_t[j,7])] <- Adj[unlist(pub_bike_t[j,3]),unlist(pub_bike_t[j,7])] +1
  Adj[unlist(pub_bike_t[j,7]),unlist(pub_bike_t[j,3])] <- Adj[unlist(pub_bike_t[j,7]),unlist(pub_bike_t[j,3])] +1
  
  #if(j %%10000 == 0){print(j)
  # }else{}
}

Adj_df <- as.data.frame(Adj)

for(j in 1:N) {
  Adj_df[j,j] <- 0
}

Adj <- as.matrix(Adj_df)

#igrph를 통해서 graph.adjacency 구성 및 cluster_fast_greedy 적ㅇ
g <- graph.adjacency(Adj, mode="undirected", weighted=T)
a <- cluster_fast_greedy(g, weights = E(g)$weight)

#나누어진 cluster 할당
index = a$membership
df = as.data.frame(cbind(a$membership,a$names))

colnames(df) <- c("commu","stnumber")
aa <- as.data.frame(pub_bike_loc)
#head(df)


pub_bike_loc_m <- merge(x = aa, y = df,by = "stnumber", all.x = TRUE) 
pub_bike_loc_m <- subset(pub_bike_loc_m, is.na(pub_bike_loc_m$commu)==0)


#head(pub_bike_loc_m)

#community가 4인 곳만 추출하기
subAdj  <- Adj_df[which(colnames(Adj_df)%in%pub_bike_loc_m[pub_bike_loc_m$commu==4,]$stnumber),]
subAdj_1  <- subAdj[,which(colnames(Adj_df)%in%pub_bike_loc_m[pub_bike_loc_m$commu==4,]$stnumber)]

#추가적인 cluster_fast_greedy 적용
sAdj <- as.matrix(subAdj_1)
g <- graph.adjacency(sAdj, mode="undirected", weighted=T)
ss <- cluster_fast_greedy(g, weights = E(g)$weight)

index_s = ss$membership
df_s = as.data.frame(cbind(ss$membership,ss$names))
colnames(df_s) <- c("commu","stnumber")
aa <- as.data.frame(pub_bike_loc)


pub_bike_loc_s <- merge(x = aa, y = df_s,by = "stnumber", all.x = TRUE) 
pub_bike_loc_s <- subset(pub_bike_loc_s, is.na(pub_bike_loc_s$commu)==0)



#지도에 시각화 

library("ggplot2")
library("sp")
library("maptools")
library("rgdal")
library("geosphere")

#좌표 변환계 설정
crsUTM_K <- CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=bessel +units=m +no_defs")
crsWGS84lonlat <- CRS("+proj=longlat +zone=52 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +lat_0=38N +lon_0=127E")

#대한민국 행정구역 shp파일 통해 지도 그ㄹ기
korea_map_shp <- readShapePoly("TL_SCCO_EMD.shp", verbose =T, proj4string =crsUTM_K)
seoul_shape <- korea_map_shp[grep("^11",korea_map_shp$EMD_CD),]
sp:::plot(seoul_shape,axes =FALSE, bty="n",border = "gray90", bg = "gray30" ,col ="gray",main = "Public Bicycle Station Communities via Modularity Optimization")

#좌표 변환 
xy.new <- spTransform(SpatialPoints(pub_bike_loc_m[3:2],proj4string = crsWGS84lonlat),crsUTM_K) 
xy.new_s <- spTransform(SpatialPoints(pub_bike_loc_s[3:2],proj4string = crsWGS84lonlat),crsUTM_K) 

#커뮤니티 index로 색깔 부여
commu_col <- as.factor(pub_bike_loc_m$commu)
col.rainbow <- rainbow(13)
palette("default")

#############
for(i in 1:length(pub_bike_loc_m[,1])){
  points(as.data.frame(xy.new[i]),pch=20, col = as.numeric(pub_bike_loc_m$commu[i]) ,cex=0.8)
}


####################################################


for(i in 1:8){
  xy.new_a <- spTransform(SpatialPoints(pub_bike_loc_m[3:2][which(pub_bike_loc_m$commu==i),],proj4string = crsWGS84lonlat),crsUTM_K) 
  sp:::plot(seoul_shape,axes =FALSE, bty="n",border = "gray90", bg = "gray30" ,col ="gray", main = i)
  
  for(j in 1:length(xy.new_s)){
    points(as.data.frame(xy.new_s[j]),pch=20, col = as.numeric(df_s$commu[j]) ,cex=0.9)
  }
}
  

write.csv(pub_bike_loc_m, "7-9월커뮤니티.csv")
write.csv(pub_bike_loc_s, "7-9월커뮤니티_4커뮤.csv")
  
  
