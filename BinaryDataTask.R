BinaryDataCorrelation<-function(ini.data){
        temp<-vector()
        set.seed(1)
        
        #Data sampling and cleaning: 
        sampled.data<-ini.data[sample(nrow(ini.data),500),]
        sampled.data<-sampled.data[,-1]
        for (i in 1:dim(sampled.data)[1]) {
                if (all(sampled.data[i,]==0))  {temp<-cbind(temp,i)}
        }        
        sampled.data<-sampled.data[-temp,]
        
        #Getting Phi coefficients from hetcor() function  
        recived.het<-hetcor(sampled.data,digits=3)
        diag(recived.het[[1]])<-0
        recived.het[[1]][lower.tri(recived.het[[1]])] <- 0
        recived.variables<-which(recived.het[[1]]>=0.5,arr.ind=T,useNames=T)
        recived.variables<-recived.variables-1
        
        #Plotting correlated and non correlated variables as the function of "id" 
        x<-seq(1:dim(sampled.data)[1])
        old.par<-par(mfrow=c(1, 2))
        plot(x,sampled.data$item_5,ylim=c(0.95,1.1),xlim=c(1,50),xlab="Sample number",ylab="item_5+item_3+item_22")
        points(x,sampled.data$item_3+0.01,col="red",type="p",pch=24)
        points(x,sampled.data$item_22+0.02,col="blue",type="p",pch=22)
        plot(x,sampled.data$item_9,ylim=c(0.95,1.1),xlim=c(1,100),xlab="Sample number",ylab="item_5+item_13")
        points(x,sampled.data$item_13+0.01,col="red",type="p",pch=24)
        par(old.par)
                
        return(list(recived.variables=recived.variables))
}