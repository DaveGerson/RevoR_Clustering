### .cmd command copy E:\Revo_R_Data\Customer_Info\* to E:\Revo_R_Data\Customer_Info\customer_info.csv
### Import data from Kaggle Competition https://www.kaggle.com/c/acquire-valued-shoppers-challenge/data

custText <- RxTextData("E:/Revo_R_Data/Customer_Info/customer_info.csv", delimiter = ",")
customerInfo_xdf<-rxImport(custText,outFile = "E:/Revo_R_Data/Customer_Info/customer_info.xdf")
customerInfo_xdf<-rxImport(custText)


scaledcustomerInfo_xdf<-scale(customerInfo_xdf)

custCLuster_k3<-rxKmeans(~Stores_Shopped + Trips + Average_Spend + Average_ItemCost ,scaledcustomerInfo_xdf,numClusters = 3)
custCLuster_k4<-rxKmeans(~Stores_Shopped + Trips + Average_Spend + Average_ItemCost ,scaledcustomerInfo_xdf,numClusters = 4)
custCLuster_k5<-rxKmeans(~Stores_Shopped + Trips + Average_Spend + Average_ItemCost ,scaledcustomerInfo_xdf,numClusters = 5)
custCLuster_k6<-rxKmeans(~Stores_Shopped + Trips + Average_Spend + Average_ItemCost ,scaledcustomerInfo_xdf,numClusters = 6)
custCLuster_k7<-rxKmeans(~Stores_Shopped + Trips + Average_Spend + Average_ItemCost ,scaledcustomerInfo_xdf,numClusters = 7)



aggregate(mydata,by=list(fit$cluster),FUN=mean)


plot(customerInfo_xdf,custCLuster_k3$cluster,asp=1)
symbols(Trips, Trips, circle = sd1 , add = TRUE, fg = "blue", lwd = 2)
symbols(Average_ItemCost, Average_ItemCost, circle = sd2 , add = TRUE, fg = "red", lwd = 2)