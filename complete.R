complete<-function(directory,id=1:332){
    old.dir<-getwd()
    new<-directory
    new.dir<-paste(old.dir,new,sep="/")
    setwd(new.dir)
    len<-length(id)
    name<-function(x){
        if(x<10)
            return(paste("00",x,".csv",sep=""))
        else if(x<100)
            return(paste("0",x,".csv",sep=""))
        else
            return(paste(x,".csv",sep=""))
    }
    p<-0
    for(i in 1:len){
        t<-read.csv(name(id[i]))
        t<-t[complete.cases(t),]
        num<-nrow(t)
        if(num>0){
            if(p==0){
                DF<-data.frame(id=id[i],nobs=num)
                p<-1
            }
            else if(p==1){
                DF<-rbind(DF,data.frame(id=id[i],nobs=num))
            }
        }
    }
    setwd(old.dir)
    DF
}