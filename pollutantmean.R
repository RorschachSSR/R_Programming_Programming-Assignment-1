pollutantmean<-function(directory,pollutant,id=1:332){
    old.dir<-getwd()
    new<-directory
    new.dir<-paste(old.dir,new,sep="/")
    setwd(new.dir)
    len<-length(id)
    s<-0
    num<-0
    name<-function(x){
        if(x<10)
            return(paste("00",x,".csv",sep=""))
        else if(x<100)
            return(paste("0",x,".csv",sep=""))
        else
            return(paste(x,".csv",sep=""))
    }
    if(pollutant=="sulfate"){
      for(i in 1:len){
          t<-read.csv(name(id[i]))
          s<-s+sum(t$sulfate,na.rm=TRUE)
          x<-as.numeric(!is.na(t$sulfate))
          num<-num+sum(x)
      }
    }
    else if(pollutant=="nitrate"){
        for(i in 1:len){
            t<-read.csv(name(id[i]))
            s<-s+sum(t$nitrate,na.rm=TRUE)
            x<-as.numeric(!is.na(t$nitrate))
            num<-num+sum(x)
        }
    }
    setwd(old.dir)
    s/num
}