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
  d2=(d2[d2>0])
  d2
}

createMatrix <- function (data, d2) {
  #create new matrix
  res = t(apply(data[,3:ncol(data)], 1, convertVals, d2 )) 
  res = as.data.frame(res)
  
  #label columns and rows
  res = cbind(data[,1], res)
  names(res)=c(unique(data[,2]),d2)
  res
}

checkLicorFormat <- function(atable){
  minBp = 3
  maxBp = 1000
  misBp = c(-1,-9)
  valBp = c(misBp, minBp:maxBp)
  if(!is.character(atable[,1])) return(FALSE)
  if(!all(unique(atable[,1]) == atable[,1])) return(FALSE)
  if(!is.character(atable[,2])) return(FALSE)
  if(length(unique(atable[,2])) != 1) return(FALSE)
  for(i in 3:ncol(atable)){
    if( all(is.na(as.integer(atable[,i])))) return(FALSE)
    atable[,i] = as.integer(atable[,i])
    if(!(all(atable[,i] %in% valBp))) return(FALSE)
  }
  return(TRUE)
}

getValidLicorTables <- function(filename){
  wb = loadWorkbook(filename)
  nms = names(getSheets(wb))
  n = length(nms)
  res = logical(n)
  out = list()
  j = 0
  for(i in 1:n){
    ok = FALSE
    try({
      data = read.xlsx2(filename, sheetName=nms[i], header=FALSE, stringsAsFactors=FALSE)
      ok = TRUE
    }, silent=TRUE)
    if(ok){
      res[i] = checkLicorFormat(data)
      if(res[i]){
        j = j + 1
        for(k in 3:ncol(data)) data[,k] = as.integer(data[,k])
        out[[j]] = data
        names(out)[j] = nms[i]
      }
      
    }
  }
  #validSheets = nms[res]
  return(out)
}

#' Reads a licor file
#' 
#' 
#' @aliases read.licor
#' @param filename expects a tab delimted text file or an excel sheet
#' @return list with filename entry and a sublist of datasets (named by sheet names or "csv" otherwise)
#' @author Reinhard Simon
#' @export
read.licor <- function(filename=NULL){
  data = NULL
  sheetName = NULL
  if(is.null(filename)) {
    Excel = c("Excel files (*.xlsx;*.xls)","*.xlsx;*.xls")
    Filters2 = rbind(Filters,Excel)
    filename = choose.files(default = "", caption = "Select licor file!",
                            filters = Filters2[c("txt","Excel"),] 
    )
  }
  
  if(length(filename)>0){
    if(str_detect(filename,".txt")){
      data = list(csv = read.csv(filename, stringsAsFactors=FALSE, header=F, sep="\t"))  
    } else {
      #data = read.xlsx2(filename, sheetName = sheetName, stringsAsFactors=FALSE, header=FALSE) 
      data = getValidLicorTables(filename)
    }
  }
  list(filename = filename, data=data)
}

#' Convert licor data into matrix of zero and one
#' 
#' Assumes a bp weight value or -1 and -9; the latter will be replaced by 0; the weight by 1.
#' The column will be renamed with the bp weight. The function also creates a file of the 
#' transformed data in .csv format within the the same path. The file name is appended with '_out'
#' and the extension set to '.csv'.
#' 
#' @aliases licor2matrix
#' @param filename an optional filename given the full path or relative path. If not given the user is asked.
#' @return list filename and data if succesful; NA if otherwise
#' @author Reinhard Simon
#' @export
licor2matrix <- function(data=NULL){
  res=NA
  n = length(data$data)
  if( n > 0 ){
    db = list(n)
    for(i in 1:n){
      d2 = identifyUniqueVals(data$data[[i]])
      trf= createMatrix(data$data[[i]], d2)
      
      #store
      #outname = file.path(dirname(filename), gsub("txt",names(res)[1],basename(filename),) )
      newdata = cbind(data$data[[i]],trf)
      names(newdata)[1] = "Genotype"
      db[[i]] = newdata
      names(db)[i] = names(data$data[i])
      res=list(filename = data$filename, data = db)
    }
  }
  res
}

getAlleleStart <-function(data){
  max(which(str_detect(names(data),"X")))+2
}

#' summarizes the marker data
#' 
#' Simple statistics for each marker: sheet name, marker name, # of genotypes, # of alleles, allele bp weight.
#' 
#' Returns a dataframe
#' 
#' @aliases write.licor
#' @param licor.res list object of licor transformation results with filename and data
#' @return data.frame
#' @author Reinhard Simon
#' @export
summary.licor <- function(licor.res){
  Sheet = ""
  Marker = ""
  Genotypes = 0
  Alleles = 0
  bp = ""
  out = as.data.frame(cbind(Sheet,Marker,Genotypes, Alleles,bp), stringsAsFactors=FALSE)
  
  n = length(licor.res$data)
  if(n > 0){
    for(i in 1:n){
      data = licor.res$data[[i]]
      aidx =  getAlleleStart(data) # Index of first allele column
      n.alleles = ncol(data)- aidx + 1
      nms.alleles = names(data)[aidx:ncol(data)]
      out[i,"Sheet"] = names(licor.res$data[i])
      out[i,"Marker"] = unique(data[,2])
      out[i,"Genotypes"] = nrow(data)
      out[i,"Alleles"] = n.alleles
      out[i,"bp"] = paste(nms.alleles,collapse=", ")
    }
    out$Genotypes = as.integer(out$Genotypes)
    out$Alleles = as.integer(out$Alleles)
  }
  out
}

#' Joins the marker data
#' 
#' Creates a final matrix of all valid sheets. If the list of genotypes differ, empty cells (NA) will be created.
#' The table will have the first column with the genotype names, then all allele columns. Column names are
#' renamed using the marker name before the allele weight and separated by a dot (e.g.: MARKER.123).
#' 
#' Returns a dataframe
#' 
#' @aliases write.licor
#' @param licor.res list object of licor transformation results with filename and data
#' @return data.frame
#' @author Reinhard Simon
#' @export
join.markers = function(licor.res, all=TRUE){
  lic = licor.res
  n = length(lic$data)
  out = NULL
  if(n > 0){
    #cycle over list of data frames
    for(i in 1:n){
    #get data frame
    data = lic$data[[i]]
    #take out raw data
    aidx = getAlleleStart(data)
    mrnm = names(data)[aidx-1]
    
    #rename columns with marker name before
    alls = names(data)[aidx:ncol(data)]
    alnm = paste(mrnm,alls,sep=".")
    names(data)[aidx:ncol(data)] = alnm
    data = data[,-c(2:(aidx-1))]
    if(i==1) out = data
    #if i > 1 merge
    if(i > 1){
      out = merge(out, data, by="Genotype", all=all)
      #out[is.na(out)] = 0
    }
    }
  }
  out
}

#' Write licor transformed data to a file
#' Assumes a bp weight value or -1 and -9; the latter will be replaced by 0; the weight by 1.
#' In case of Excel formats the function adds a summary sheet of the different markers, and a joint table
#' if the total column number is less than 256.
#' 
#' @aliases write.licor
#' @param licor.res list object of licor transformation results with filename and data
#' @author Reinhard Simon
#' @export
write.licor <- function(licor.res) {
  lic = licor.res
  n =length(lic$data) 
  if(n > 0){
    filename = lic$filename
    if(names(lic$data)[1] == "csv"){
      write.csv(res$data,filename, row.names=F)    
    } else { # assume various sheets from excel file
      wb = loadWorkbook(filename)
      removeSheet(wb, "Summary Licor data")
      saveWorkbook(wb,filename)
      for(i in 1:n){
        wb = loadWorkbook(filename)
        sheet = names(lic$data[i])
        removeSheet(wb, sheet)
        saveWorkbook(wb,filename)
        write.xlsx2(lic$data[[i]] ,filename, sheetName = sheet,row.names=F, append=T)    
      }
      write.xlsx2(summary.licor(lic), filename, sheetName = "Summary Licor data",row.names=F, append=T)
      join = join.markers(lic)
      wb = loadWorkbook(filename)
      removeSheet(wb, "Joined icor data")
      saveWorkbook(wb,filename)
      if(ncol(join)<256) {
        write.xlsx2(join, filename, sheetName = "Joined licor data",row.names=F, append=T)
      }
    }
    
  }
}

                        

#' Presents the packages graphical user interface
#'
#' Runs a web server to show the user interface.
#' 
#' 
#' @aliases runLicorMatrix
#' @param port the port where to listen; 1972 by default.
#' @author Reinhard Simon
#' @family interface
#' @export
runLicorMatrix <- function(port = 1972L) {
  shiny::runApp(system.file("www", package = "licor"), port = port)
} 


