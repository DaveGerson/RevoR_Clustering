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

scaled_customerClusterInfo<-rxDataStepXdf(customerInfo_xdf,removeMissings = TRUE,varsToKeep = c("Trips","Average_Spend","Average_ItemCost") )
library("psych", lib.loc="C:/Revolution/R-Enterprise-7.0/R-3.0.2/library")
#Winsorizing data to remove "heavy outliers" from analysis
scaled_customerClusterInfo<-data.frame(winsor(scaled_customerClusterInfo,trim=.05))
##Scale data so the clustering doesn't favor the "larger" numerical datapoints.
scaled_customerClusterInfo<-data.frame(scale(scaled_customerClusterInfo))

names(scaled_customerClusterInfo)<-c("Trips_Scaled","Average_Spend_Scaled","Average_ItemCost_Scaled")
customerInfo_xdf<-rxImport(scaled_dataloc)

#builds xdf file of scaled data
scaled_dataloc = "E:/Revo_R_Data/Customer_Info/scaled_customer_data.xdf"
rxMerge(customerInfo_xdf,scaled_customerClusterInfo,outFile = scaled_dataloc,type="oneToOne",overwrite=TRUE)
customerInfo_xdf<-rxImport(scaled_dataloc)


#Computing total sum of squares to demo Calinski Harabasz Index
gmean<-apply(scaled_customerClusterInfo,2,FUN=mean)
sqr_edist <- function(x, y) { sum((x-y)^2) }  #Euclidean Distance Calculator
totss<-sum(apply(scaled_customerClusterInfo,1,FUN=function(row){sqr_edist(row,gmean)}))
custcount<-dim(customerInfo_xdf)[1]

#prep for kmeans loop
cluster_WSS_diag<- c()
cluster_BSS_diag<-c()
chnumerator<-c()
chdenominator<- c()
customerClusters<-customerInfo_xdf
k<-1
for (k in 1:10){
##Actual Generation of the clusters
custCluster<-rxKmeans(~Trips_Scaled + Average_Spend_Scaled + Average_ItemCost_Scaled,customerInfo_xdf,numClusters = k)

##Actual Generation of the clusters
aggregate(customerInfo_xdf,by=list(custCluster$cluster),FUN=mean)
aggregate(customerInfo_xdf,by=list(custCluster$cluster),FUN=median)
#Cluster Daiagnostics based on within cluster variance.   End Goal is to implement Calinski Harabasz
cluster_WSS_diag<-union(cluster_WSS_diag,custCluster$tot.withinss)
cluster_BSS_diag<-union(cluster_BSS_diag,totss-cluster_WSS_diag[k])
chnumerator<-union(chnumerator,cluster_BSS_diag[k]/k-1)
chdenominator<-union(chdenominator,(cluster_WSS_diag[k]/(custcount-k)))

customerClusters <- data.frame(customerClusters, custCluster$cluster)
names(customerClusters)[10+k]<-paste("Cluster",k,sep="")
}

scoringList<-list(CH = chnumerator/chdenominator, wss = cluster_WSS_diag, tss = totss  )

#Plotting Calinski Harabasz Index
plot(scoringList$wss,type="l",col="blue")
par(new=TRUE)
plot(scoringList$CH,type="l",col="red")

#The "Elbow" of my data shows a peak at 4 so I am choosing that as my default number of clusters
cluster_outfile<-"E:/Revo_R_Data/Customer_Info/customer_clusters.csv"
customerClusters_xdf<-rxImport(customerClusters)
rxDataStep(customerClusters,outFile=cluster_outfile,overwrite=TRUE,varsToDrop = c("Trips_Scaled","Average_Spend_Scaled","Average_ItemCost_Scaled"))

#Visualization of Cluster Data at Tableau Public
#http://public.tableausoftware.com/profile/ucgerson#!/vizhome/ClusterDiagnosticForKaggleCustomerValueChalleng/ClusterDash
