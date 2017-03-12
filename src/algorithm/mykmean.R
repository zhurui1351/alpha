
My_kmeans<-function(data,k,max.iter=10){

  mydata = as.matrix(data)
  rows<-nrow(mydata) #获取行数
  cols<-ncol(mydata) #获取列数

  within<-matrix(0,nrow=k,ncol=1) #within存储类内距离平方和
  between<-0#类间距离
  iter=0#迭代次数


  #定义indexMatrix矩阵,第一列为每个数据所在的类，第二列为每个数据到其类中心的距离
  indexMatrix<-matrix(0,nrow=rows,ncol=2)
  indexMatrix[,2] = Inf
  centers<-matrix(0,nrow=k,ncol=cols) #centers矩阵存储类中心
  randSeveralInteger<-as.vector(sample(1:rows,size=k))


  #通过生成随机数的方式，得到初始的聚类中心
  for(i in 1:k){
    indexMatrix[randSeveralInteger[i],1]<-i
    centers[i,]<-data[randSeveralInteger[i],]
    centers<-matrix(centers,k,cols)
  }

  changed=TRUE #changed标记数据所在类是否发生变化
  while(changed){ #当存在数据所在的类发生变化时
  
    if(iter>=max.iter)
      break
  
    #print(iter)
    changed=FALSE
  
    #对每一个数据，计算其到各个类中心的距离，并将其划分到距离最近的类
    for(i in 1:rows){ 
      initialDistance<-indexMatrix[i,2] #初始距离
      previousCluster<-indexMatrix[i,1]#previousCluster表示该数据之前所在类
    
      #遍历所有的类，将该数据划分到距离最近的类
      for(j in 1:k){ 
       # currentDistance<-(sum((/data[i,]-centers[j,])^2))^0.5#计算该数据到类j的距离
        currentDistance = 1-cor(as.numeric(data[i,]),as.numeric(centers[j,]))
        if(currentDistance < initialDistance)#如果该数据到类j的距离更近
        {
          initialDistance<-currentDistance #更新initialDistance的值
          indexMatrix[i,1]<-j #认为该数据属于类j
          indexMatrix[i,2]<-currentDistance  #更新该数据到类中心的距离
        }
          
      } 
    
  
  #如果该数据所属的类发生了变化，则将changed设为TRUE，算法继续
    if(previousCluster!=indexMatrix[i,1])
    {
      changed=TRUE
    }
  }

  #重新计算类中心 
  for(m in 1:k){
    clusterMatrix<-data[indexMatrix[,1]==m,] #得到属于第m个类的所有数据
    clusterMatrix<-as.matrix(clusterMatrix)
    if(nrow(clusterMatrix)>0){ #如果属于第m类的数据的数目大于0  
      centers[m,]<-colMeans(clusterMatrix) #更新第m类的类中心
    } 
    else{
      centers[m,]<-centers[m,] #否则，第m类的类中心不发生变化
    }    
  }
  iter=(iter+1)#迭代次数＋1
  }

  #开始计算函数返回值
 # ss<-function(x) sum(scale(x,scale=FALSE)^2)
#  between<-ss(centers[indexMatrix[,1],])#between存储类间差异
 # within<-sapply(split(as.data.frame(data),indexMatrix[,1]),ss)#within存储类内差异
  #twithin<-sum(within) #total within-clustersum of squares

#生成返回值列表cluster,tot.withinss,betweenss,iteration
  #result<-list(cluster=indexMatrix[,1],tot.withinss=twithin,betweenss=between,iteration=iter,centers=centers)
  result<-list(cluster=indexMatrix[,1],iteration=iter,centers=centers)

  return(result) 
}

sin_sample = function(n=15,scale=0)
  {
    return(sin(seq(0,2*pi + jitter(scale) ,length.out=n)) * rnorm(n)) 
  }

get_sin_sample = function(row = 20,n=15,scale=0)
{
  result = data.frame()
  for(i in 1:row)
  {
    r = matrix(sin_sample(n,scale),nrow=1)
    result = rbind(result,r)
  }
  return(result)
}

cluster = function(xx,center_num=10,isplot=F,seed=1234)
{
  if(seed !='')
  {
    set.seed(seed)
  }
  xx_scaled = scale(xx)
  fit = kmpp(xx_scaled,center_num,iter.max = 50000,nstart=100)
  centers = fit$centers
  labels = fit$cluster
  
  centers_unscaled = unscale(centers,xx_scaled) 
  centers = centers_unscaled
  if(isplot)
  {
    windows(1000,1000)
    
    plot(centers[1,],type='l',ylim = range(max(centers),min(centers)),xlab='',xaxt = 'n') #ylim = range(-6,6)
    
    axis(1, 1:length(centers[1,]),names(centers[1,]))
    for( i in 2:n)
    {
      points(centers[i,],type='l',col=i)
    }    
  }  
  
  return(list(centers=centers,labels=labels ))
}


kmpp <- function(X, k,iter.max = 50000, nstart = 100) { 
  set.seed(1234)  
  n <- nrow(X) 
  C <- numeric(k) 
  C[1] <- sample(1:n, 1) 
  
  for (i in 2:k) { 
    dm <- distmat(X, X[C, ]) 
    pr <- apply(dm, 1, min); pr[C] <- 0 
    C[i] <- sample(1:n, 1, prob = pr) 
  } 
  
  return(kmeans(X, X[C, ],iter.max = 50000, nstart = 100) )
} 

