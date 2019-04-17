#Importing Data
telco<-read.csv("C:/Users/Ravinder/Desktop/Segmentation/telco_csv.csv")

#Exploring data
View(telco)
str(telco)
names(telco)


#function for creating descriptive statistics

mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  pctls<-quantile(a,probs=c(0.01, 0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
  max <- max(a)
  return(c(n=n, nmiss=nmiss,  mean=m, stdev=s,min = min, pctls=pctls,max=max))
}

vars <- c( "region","tenure" ,"age" ,"marital" ,"address" , "income","ed" ,"employ",
           "retire","gender","reside","tollfree", "equip","callcard", "wireless", "longmon",
           "tollmon",  "equipmon", "cardmon",  "wiremon",  "multline", "voice", "pager" ,
           "internet","callid", "callwait", "forward", "confer", "ebill","custcat")

diag_stats<-t(data.frame(sapply(telco,mystats)))  

#write.csv(diag_stats, "C:/Users/Ravinder/Desktop/Data Science/Factor Analysis & Segmentation/diag_stats.csv")

#Outliers - capping at mean+3sd (no capping done on lower side due to negative numbers)
telco$longmon[telco$longmon>42.81355889]<- 42.81355889
telco$tollmon[telco$tollmon>63.98036623]<- 63.98036623
telco$equipmon[telco$equipmon>71.42541641]<- 71.42541641
telco$cardmon[telco$cardmon>56.03448901]<- 56.03448901
telco$wiremon[telco$wiremon>70.74217684]<- 70.74217684
telco$income[telco$income>232.25]<- 232.25

#View(telco)

inputdata_final <-telco[vars]

#Prepare final Data
#standardizing the data

inputdata_final = data.frame(scale(inputdata_final))

clus <- c("tollmon",
          "callwait",
          "forward",
          "pager",
          "voice",
          "equipmon",
          "internet",
          "tenure",
          "longmon",
          "multline",
          "income",
          "wiremon",
          "cardmon")

inputdata_clus <- inputdata_final[clus]

#View(inputdata_final)
#building clusters using k-means clustering 

cluster_three <- kmeans(inputdata_clus,3)
cluster_four <- kmeans(inputdata_clus,4)
cluster_five <- kmeans(inputdata_clus,5)
cluster_six <- kmeans(inputdata_clus,6)


cluster_three$cluster


telco_new<-cbind(telco,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )
View(telco_new)

#Graph based on k-means
install.packages('cluster')
require(cluster)

clusplot(inputdata_clus, #dataframe
         cluster_three$cluster, #clusterdata
         color = TRUE, #color
         #shade = TRUE, # Lines in clusters
         lines =6, # lines connecting centroids
         labels = 2 # Labels clusters and cases
)

###Profiling

#Converting into factors
telco_new$km_clust_3=factor(telco_new$km_clust_3)
telco_new$km_clust_4=factor(telco_new$km_clust_4)
telco_new$km_clust_5=factor(telco_new$km_clust_5)
telco_new$km_clust_6=factor(telco_new$km_clust_6)

install.packages('tables')
require(tables)


profile<-tabular(1+tenure+age+marital+address+income+ed+employ+retire+gender+reside+tollfree+
                   equip+callcard+wireless+longmon+tollmon+equipmon+cardmon+wiremon+multline+
                   voice+pager+internet+callid+callwait+forward+confer+ebill ~ mean +
                   (mean*km_clust_3)+(mean*km_clust_4)+(mean*km_clust_5)+(mean*km_clust_6),
                 data=telco_new)
profile1<-as.matrix(profile)

profile1<-data.frame(profile1)
View(profile1)

profile<-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6),
                 data=telco_new)

profile2<-as.matrix(profile)
profile2<-data.frame(profile2)
View(profile2)

write.csv(profile1,"profile1.csv",row.names = F)
write.csv(profile2,"profile2.csv",row.names = F)
#############################END OF k-Means Segmentation############################


