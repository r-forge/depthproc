depth = function(u, X, method="Projection", ndir=1000, digits=2)
{
  if(is.data.frame(u)) u = as.matrix(u)
  if(is.data.frame(X)) X = as.matrix(X)
  
  set.seed(1)
  if(is.vector(X)) X = matrix(X,ncol = 1)
  if(is.vector(u)) u = matrix(u,ncol = dim(X)[2])
  
  #######################################################################
  if (method=="Mahalanobis")
  {  
    cov = cov(X)
    center = colMeans(X)
    
    icov = solve(cov)
    
    d <- function(u,center = center, icov = icov)
    {
      depth<-1/(1+t(u-center)%*%icov%*%(u-center))
      depth	
    }
    
    depth = apply(u,1,d,center,icov)         
  }
  
  ######################################################################
  if (method=="Euclidean")
  {
    n = dim(u)[1]
    
    center = colMeans(X)
    center = matrix(rep(center,n),nrow=n,byrow=TRUE)
    depth=1/(1+(rowSums((u-center)^2)))	
  }
  
  ####################################
  if (method=="Projection")  
  {
    
    projection_depth_1d =function(u,X, location=median, scale=mad)
    {
      depth = 1/(1+abs(u-location(X))/scale(X))
    }
    
    if (ncol(X)==1)
    {
      depth=projection_depth_1d(u,X)
    }
    else
    {
      
      proj = runifsphere(ndir, ncol(X))
      
      xut = tcrossprod(X,proj)
      uut = tcrossprod(u,proj)
      
      tmp = .Call("colMediansMads",PACKAGE = "depthproc",xut,nrow(xut),ncol(xut))
      
      MED = tmp[1:ncol(xut)]
      MAD = tmp[(ncol(xut)+1):(2*ncol(xut))]
      
      
      OD<-(1/(1+(abs(t(uut)-MED)/MAD)))
      
      depth<-apply(OD,2,min)
      depth
    }
  }
  
  
  #######################################################################
  if (method=="Tukey")
  {
    
    tukey1d = function(u,X)
    {
      Xecdf = ecdf(X)
      uecdf = Xecdf(u) 
      uecdf2 = 1-uecdf
      min.ecdf = uecdf>uecdf2
      depth = uecdf 
      depth[min.ecdf]=uecdf2[min.ecdf] 
      depth
    }
    
    if (ncol(X)==1)
    {
      depth= tukey1d(u,X)
    }
    
    #### 
    else  # czyli jesli wymiar d>2
    {
      
      
      proj = t(runifsphere(ndir, ncol(X)))
      xut = X%*%proj
      uut = u%*%proj
      
      OD<-matrix(nrow=nrow(uut),ncol=ncol(uut))
      
      for (i in 1:ndir)
      {
        
        OD[,i]=tukey1d(uut[,i],xut[,i])  
      }
      
      depth<-apply(OD,1,min)
      
      rm(.Random.seed, pos = 1)
    }
  }
  
  
  ########################################################
  
  if (method=="Liu")
  {
      liu1d<-function(u,X){
      dys<-ecdf(X)
      depth<-dys(u)*(1-dys(u))
      depth
    }
    
    
    if (ncol(X)==1)
    {
      depth= liu1d(u,X)
    }
    
    #### 
    else  # czyli jesli wymiar d>2
    {
      
      
      proj = t(runifsphere(ndir, ncol(X)))
      xut = X%*%proj
      uut = u%*%proj
      
      OD<-matrix(nrow=nrow(uut),ncol=ncol(uut))
      
      for (i in 1:ndir)
      {       
        OD[,i]=liu1d(uut[,i],xut[,i])  
      }
      
      depth<-apply(OD,1,min)
      
      rm(.Random.seed, pos = 1)
    }
  }
  
  ########################################################
  depth
}




