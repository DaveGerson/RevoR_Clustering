### .cmd command copy E:\Revo_R_Data\Customer_Info\* to E:\Revo_R_Data\Customer_Info\customer_info.csv
### Import data from Kaggle Competition https://www.kaggle.com/c/acquire-valued-shoppers-challenge/data

custText <- RxTextData("E:/Revo_R_Data/Customer_Info/customer_info.csv", delimiter = ",")
customerInfo_xdf<-rxImport(custText,outFile = "E:/Revo_R_Data/Customer_Info/customer_info.xdf",overwrite=TRUE)
customerInfo_xdf<-rxImport(custText)
str(customerInfo_xdf)
names(customerInfo_xdf)<-c("ID","Stores_Shopped","Trips","SalesDollars","ItemsPurchased","Average_Spend","Average_ItemCost")


##Remove Nulls and Properly format Data  
customerInfo_xdf<-rxDataStepXdf(customerInfo_xdf,removeMissings = TRUE)
#customerClusterInfo_xdf<-rxDataStepXdf(customerInfo_xdf,removeMissings = TRUE,varsToKeep = c("ID","Trips","Average_Spend","Average_ItemCost") )

##Scals data so the clustering doesn't favor the "larger" numerical datapoints.
scaled_customerClusterInfo<-rxDataStepXdf(customerInfo_xdf,removeMissings = TRUE,varsToKeep = c("Trips","Average_Spend","Average_ItemCost") )
scaled_customerClusterInfo<-data.frame(scale(scaled_customerClusterInfo))
names(scaled_customerClusterInfo)<-c("Trips_Scaled","Average_Spend_Scaled","Average_ItemCost_Scaled")
customerInfo_xdf<-rxImport(scaled_dataloc)

#builds xdf file of scaled data
scaled_dataloc = "E:/Revo_R_Data/Customer_Info/scaled_customer_data.xdf"
rxMerge(customerInfo_xdf,scaled_customerClusterInfo,outFile = scaled_dataloc,type="oneToOne",overwrite=TRUE)
customerInfo_xdf<-rxImport(scaled_dataloc)

##Actual Generation of the clusters
custCluster_k1<-rxKmeans(~Trips_Scaled + Average_Spend_Scaled + Average_ItemCost_Scaled,customerInfo_xdf,numClusters = 1)
custCluster_k2<-rxKmeans(~Trips_Scaled + Average_Spend_Scaled + Average_ItemCost_Scaled,customerInfo_xdf,numClusters = 2)
custCluster_k3<-rxKmeans(~Trips_Scaled + Average_Spend_Scaled + Average_ItemCost_Scaled,customerInfo_xdf,numClusters = 3)
custCluster_k4<-rxKmeans(~Trips_Scaled + Average_Spend_Scaled + Average_ItemCost_Scaled ,customerInfo_xdf,numClusters = 4)
custCluster_k5<-rxKmeans(~Trips_Scaled + Average_Spend_Scaled + Average_ItemCost_Scaled ,customerInfo_xdf,numClusters = 5)
custCluster_k6<-rxKmeans(~Trips_Scaled + Average_Spend_Scaled + Average_ItemCost_Scaled ,customerInfo_xdf,numClusters = 6)
custCluster_k7<-rxKmeans(~Trips_Scaled + Average_Spend_Scaled + Average_ItemCost_Scaled ,customerInfo_xdf,numClusters = 7)


##Actual Generation of the clusters
aggregate(customerInfo_xdf,by=list(custCluster_k3$cluster),FUN=mean)
aggregate(customerInfo_xdf,by=list(custCluster_k3$cluster),FUN=median)
aggregate(customerInfo_xdf,by=list(custCluster_k4$cluster),FUN=mean)
aggregate(customerInfo_xdf,by=list(custCluster_k4$cluster),FUN=median)
aggregate(customerInfo_xdf,by=list(custCluster_k5$cluster),FUN=mean)
aggregate(customerInfo_xdf,by=list(custCluster_k5$cluster),FUN=median)
aggregate(customerInfo_xdf,by=list(custCluster_k6$cluster),FUN=mean)
aggregate(customerInfo_xdf,by=list(custCluster_k6$cluster),FUN=median)
aggregate(customerInfo_xdf,by=list(custCluster_k7$cluster),FUN=mean)
aggregate(customerInfo_xdf,by=list(custCluster_k7$cluster),FUN=median)


#Cluster Daiagnostics based on within cluster variance.   End Goal is to implement Calinski Harabasz
cluster_WSS_diag<-c(custCluster_k1$tot.withinss,custCluster_k2$tot.withinss,custCluster_k3$tot.withinss,custCluster_k4$tot.withinss,custCluster_k5$tot.withinss,custCluster_k6$tot.withinss,custCluster_k7$tot.withinss)
plot(cluster_WSS_diag,type="l")
	#Computing total sum of squares to demo Calinski Harabasz Index
gmean<-apply(scaled_customerClusterInfo,2,FUN=mean)
sqr_edist <- function(x, y) { sum((x-y)^2) }  #Euclidean Distance Calculator
totss<-sum(apply(scaled_customerClusterInfo,1,FUN=function(row){sqr_edist(row,gmean)}))
cluster_BSS_diag<-c(totss-cluster_WSS_diag[1],totss-cluster_WSS_diag[2],totss-cluster_WSS_diag[3],totss-cluster_WSS_diag[4],totss-cluster_WSS_diag[5],totss-cluster_WSS_diag[6],totss-cluster_WSS_diag[7])
custcount<-dim(customerInfo_xdf)[1]
chnumerator<-c(cluster_BSS_diag[1]/0,cluster_BSS_diag[2]/1,cluster_BSS_diag[3]/2,cluster_BSS_diag[4]/3,cluster_BSS_diag[5]/4,cluster_BSS_diag[6]/5,cluster_BSS_diag[7]/6)
chdenominator<- c(cluster_WSS_diag[1]/(custcount-1),cluster_WSS_diag[2]/(custcount-2),cluster_WSS_diag[3]/(custcount-3),cluster_WSS_diag[4]/(custcount-4),cluster_WSS_diag[5]/(custcount-5),cluster_WSS_diag[6]/(custcount-6),cluster_WSS_diag[7]/(custcount-7))
scoringList<-list(CH = chnumerator/chdenominator, wss = cluster_WSS_diag, tss = totss  )
	#Unifying scale of data and implementing clusters and plotting with GG Plot
scaled_scoringList<-data.frame(k=1:7,ch=scale(scoringList$CH),wss=scale(scoringList$wss))
scaled_scoringList<-melt(scaled_scoringList, id.vars=c("k"), variable.name="measure",value.name="score")
ggplot(scaled_scoringList, aes(x=k, y = score , color = measure)) + geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) + scale_x_continuous (breaks=1:7,labels=1:7)

plot(scoringList$wss,type="l",col="blue")
par(new=TRUE)
plot(scoringList$CH,type="l",col="red")

#The "Elbow" of my data shows a peak at 3 soi I am choosing that as my number of clusters
cluster_outfile<-"E:/Revo_R_Data/Customer_Info/customer_clusters.csv"
customerClusters_xdf <- data.frame(customerInfo_xdf, custCluster_k3$cluster)
rxDataStep(customerClusters_xdf,outFile=cluster_outfile,,overwrite=TRUE,varsToDrop = c("Trips_Scaled","Average_Spend_Scaled","Average_ItemCost_Scaled"))