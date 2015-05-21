#the correlation test

pwcorr<-function (x,type='below',digits=3,star=3,...) {  #notice that the type is not method
  require(psych)
  if (star==3) {
    mystars<-function (p) {
      ifelse (p<0.01,'***',
              ifelse (p<0.05,'**',
                      ifelse(p<0.10,'*',' ')))
    }
  }
  if (star==4){
    mystars<-function (p) {
      ifelse (p<0.001,'***',
              ifelse (p<0.01,'**',
                      ifelse(p<0.05,'*',
                             ifelse (p<0.1,'.',' '))))
    }
  }
  
  
  single<-function (...) {
    corr<-as.data.frame(corr.test(x,...)$r)
    corp<-as.data.frame(apply(corr.test(x,...)$p,2,mystars),stringsAsFactors=F)
    res<-as.data.frame(mapply(paste,round(corr,digits=digits),corp,sep=''),
                       stringsAsFactors=F)
    rownames(res)<-colnames(res)
    
    res
  }
  
  
  if (type=='two') {   #1.make the res display  two-way triangle
    
    res<-single(method='pearson')
    singres2<-single(method='spearman')
    
    #to make the res matrix become two-way triangle
    N<-ncol(res)
    for (i in seq(N)) {
      res[i,i] <- ' '
      res[0:(i-1),i]<-singres2[0:(i-1),i]
    }
    
    #res[upper.tri(res)]<-singres2[upper.tri(singres2)]   :second approach to handle
    
  } else {
    
    
    #2. display just one triangle
    
    res<-single(...)
    #to make the res matrix become triangle
    N<-ncol(res)
    if (type=='below') {
      for (i in seq(N)) {
        
        res[-c(i:N),i]<-' '
        res[i,i]<-'1.000'
      }
      #res[upper.tri(res)]<-' '
    }
    
    if (type=='above') {
      for (i in seq(N)) {
        
        res[c(i:N),i]<-' '
        res[i,i]<-'1.000'
      }
      #res[lower.tri(res)]<-' '  
    }
    
  }  
  
  res
}
