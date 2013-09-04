# convert to zero or one
convertVals = function(vals, refs){
  d3 = refs %in% vals
  d3 = as.numeric(d3) 
  d3
}

#identify all unique values ignoring -1 and -9 (or anything below 0)
identifyUniqueVals <- function (data) {
  d1= data[,c(3:ncol(data))]
  d2=sort(unique(unlist(mapply(unique, d1))))
  d2=d2[d2>0]
  d2
}

createMatrix <- function (data, d2) {
  #create new matrix
  res = t(apply(data[,3:ncol(data)], 1, convertVals, d2 )) 
  res = as.data.frame(res)
  
  #label columns and rows
  res = cbind(data[,1], res)
  names(res)=c("Genotype",paste(unique(data[,2]),d2,sep="."))
  res
}



#' Convert licor data into matrix of zero and one
#' 
#' Assumes a bp weight value or -1 and -9; the latter will be replaced by 0; the weight by 1.
#' The column will be renamed with the bp weight. The function also creates a file of the 
#' transformed data in .csv format within the the same path. The file name is appended with '_out'
#' and the extension set to '.csv'.
#' 
#' @param filename an optional filename given the full path or relative path. If not given the user is asked.
#' @return transformed data if succesful; NA if otherwise
#' @author Reinhard Simon
#' @export
licor2matrix <- function(filename=NULL){
  if(is.null(filename)) {
    filename = choose.files(default = "", caption = "Select licor file!",
                            filters = c("Licor data","*.txt") )
  } 
  res=NA
  if(length(filename)>0){
    data = read.csv(filename, stringsAsFactors=FALSE, header=F, sep="\t")
    
    d2 = identifyUniqueVals(data)
    res= createMatrix(data, d2)
    
    #store
    outname = file.path(dirname(filename), gsub(".txt","_out.csv",basename(filename),) )
    write.csv(res,outname, row.names=F)
  }
  res
}




