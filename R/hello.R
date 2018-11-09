#' @title Data Set Manipulation
#'
#' @description This package allows unstack and transpose an data set.
#'
#' @param data
#'
#' @return NULL
#'
#' @examples NULL
#'
#' @export

unstack2=function(data){

  # Verify condition
  if(dim(data)[2]!=2)
    stop("Data frame must have two columns")


  # Arguments
  data[,1]=as.character(data[,1])
  tab=table(data[,1])
  variable=names(tab)
  n=max(tab)

  # Auxiliary function
  f=function(i){
    ds=data[which(data[,1]==variable[i]),]
    dfr=as.data.frame(matrix(NA,1,(n+1)))
    dfr[1,1]=ds[1,1]
    for(j in 1:nrow(ds)) dfr[1,(j+1)]=ds[j,2]
    colnames(dfr)[1]="Variable"
    colnames(dfr)[2:(n+1)]=paste(colnames(data)[2],1:n,sep="")
    return(dfr)
  }

  # Reorder
  data.set=f(1)
  for(i in 2:length(variable))  data.set=rbind(data.set,f(i))
  return(data.set)

}

transpose=function(data){
  ds=as.data.frame(matrix(NA,(ncol(data)-1),(nrow(data)+1)))
  ds[,1]=colnames(data)[-1]
  for(j in 1:nrow(data)){
    for(i in 1:(ncol(data)-1)){
      ds[i,(j+1)]=data[j,(i+1)]
    }
  }
  colnames(ds)=c("Variable",as.character(data[,1]))
  return(ds)
}
