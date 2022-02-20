corr<-function(directory,threshold=0){
    old.dir<-getwd()
    new<-directory
    new.dir<-paste(old.dir,new,sep="/")
    setwd(new.dir)
    name<-function(x){
        if(x<10)
            return(paste("00",x,".csv",sep=""))
        else if(x<100)
            return(paste("0",x,".csv",sep=""))
        else
            return(paste(x,".csv",sep=""))
    }
    p<-0
    for(i in 1:332){
        t<-read.csv(name(i))
        t<-t[complete.cases(t),]
        num<-nrow(t)
        if(num>threshold){
            if(p==0){
                cr<-c(cor(t$sulfate,t$nitrate))
                p<-1
            }
            else if(p==1){
                cr<-c(cr,cor(t$sulfate,t$nitrate))
            }
        }
    }
    setwd(old.dir)
    cr
}