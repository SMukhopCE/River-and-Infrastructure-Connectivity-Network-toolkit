#' @title Reads NHDplus vector attributes from directory 
#' @description Reads NHDFlowline shapefile, PlusFlowlineVAA, PlusFlow, PlusARPointEvent,
#'  PlusFlowAR databasefiles and returns a large list 
#' @param filepath path to NHD data directory
#' 
#' @import rgdal 
#' @import foreign 
#' @return
#' @examples
readNHDdata <- function(filepath){
  # Assumes filepath has the following subfolders 
  # 
  NHDFlowline <- rgdal::readOGR(paste0(filepath,'NHDSnapshot/Hydrography'),'NHDFlowline',
                                use_iconv = TRUE)
   # NHDPlusv2 attribute
  PlusFlowlineVAA <- foreign::read.dbf(paste0(filepath,'NHDPlusAttributes/PlusFlowlineVAA.dbf'))
  PlusFlow <- foreign::read.dbf(paste0(filepath,'NHDPlusAttributes/PlusFlow.dbf'))
  
  ARPointEvent <- foreign::read.dbf(paste0(filepath,'NHDPlusAttributes/PlusARPointEvent.dbf'))
  PlusFlowAR <- foreign::read.dbf(paste0(filepath,'NHDPlusAttributes/PlusFlowAR.dbf'))
  
   
  return(list(NHDFlowline = NHDFlowline,PlusFlowlineVAA = PlusFlowlineVAA, 
              PlusFlow = PlusFlow,ARPointEvent = ARPointEvent, PlusFlowAR = PlusFlowAR  ))
}


#' @title  Downloads NHDPlusv2 data from FTP server 
#' @description  Download NHDPLus data by VPU number to directory given by dir_name
#'  downloads data for NHDPlusV21. For latest release of the data, update nn. 
#'  Only downloads NHDSnapshot and NHDPlusAttributes unless download.all = TRUE. 
#'  Supported VPUs :  "01 Northeast", "02 Mid Atlantic", "03N South Atlantic North",
#'   "03S South Atlantic South", "03W South Atlantic West", "04 Great Lakes",
#'  "05 Ohio",  "06 Tennessee",  "07 Upper Mississippi",  "08 Lower Mississippi",
#'  "09 Souris-Red-Rainy",  "10U Upper Missouri", "10L Lower Missouri", "11 Ark-Red-White",
#'  "12 Texas", "13 Rio Grande","14 Upper Colorado", "15 Lower Colorado",
#'  "16 Great Basin",  "17 Pacific Northwest", "18 California", 
#'  "20 Hawaii",  "21 Puerto Rico/U.S. Virgin Islands", "22A American Samoa",
#'  "22G Guam", "22M Northern Mariana Islands" 
#'  
#' @param dd drainage ID e.g. "NE". If NULL, data is downloaded for all drainage IDs (Not recommended)  
#' @param zone_num  VPUs e.g. "01"
#' @param nn Version number in the FTP 
#' @param mainurl url for NHD data download
#' @param download.all If TRUE all files are downloaded from the remote server
#' 
#' @return dataset A list of data frame containing paths of the downloaded files 
#' 
#' @import downloader
#' @import archive
#' @import RCurl 
#' @import stringr 
#' 
#' @examples
#' \dontrun{
#' get_data(dd = "MS")
#' get_data(zone_num = "01")
#' }
#' 
#' @noRd
#'

get_data <- function(dd = NULL,zone_num=NULL,out.dir = NULL, 
                     nn = 1,mainurl = NULL, 
                     download.all = FALSE){
  
  load("scripts/nhd_def.RDA")
  
  if(is.null(out.dir)){
    out.dir <- getwd()
  }
  
  if(is.null(dd) & is.null(zone_num)){
    # Look for ALL drainage ids 
    dd <- nhd_def$Drainage.Id
  }
  
  # Main url : FTP of horizon system 
  # May be user supplied latest FTP 
  if (is.null(mainurl)){
    # Accessed on 4/10/2019 
    mainurl <- "ftp://ftp.horizon-systems.com/NHDplus/NHDPlusV21/Data/" 
  }
  
  # get VPUid 
  if(is.null(zone_num)){
    # extract zone names 
    thiszone <- nhd_def$VPUs[match(dd,nhd_def$Drainage.Id)] # VPUids   
    thiszone <- unlist(strsplit(thiszone,', ')) 
  } else {
    thiszone <- zone_num # Class : Character. It can be a vector or scalar 
  }
  
  
  if(is.null(zone_num)){
    # create url paths for data download 
    folderpath <- c()
    for (idd in 1:length(dd)) {
      tmpzones <- nhd_def$VPUs[match(dd[idd],nhd_def$Drainage.Id)]
      tmpzones <- unlist(lapply(strsplit(tmpzones,', '), function(x) gsub(" ","",x) ))
      
      if (length(tmpzones) > 1) {
        tmp <- paste0(paste0(mainurl,"NHDPlus",dd[idd],"/NHDPlus"),tmpzones,"/") 
      } else {
        tmp <- paste0(mainurl,"NHDPlus",dd[idd],"/") 
      }
      folderpath <- c(folderpath,tmp)
      rm(tmp)
    }
  } else {
    folderpath <- paste0(paste0(mainurl,"NHDPlus",dd,"/NHDPlus"),thiszone,"/") 
  }
  myOpts <- curlOptions(connecttimeout = 100000)
  
  dataset <- list() 
  
  for( i in 1:length(folderpath)){
    
    # tryCatch(curlPerform(url = folderpath[i]),
    #          COULDNT_RESOLVE_HOST=function(x) cat("resolve problem\n"),
    #          error = function(x) cat(class(x), "got in\n"))
    
    filenames <- getURLContent(folderpath[i],
                               ftp.use.epsv = FALSE,
                               ftplistonly = TRUE, crlf = TRUE,
                               .opts = myOpts)
    
    
    filenames <- unlist(strsplit(filenames,"\r\n")) 
    
    filenames <- sapply(filenames, function(x) tail(unlist(strsplit(x," ")),1)) 
    
    
    # Interested in filenames containing "NHDSnapshot","WBDSnapshot" and
    #"NHDPlusAttributes" only!. Comment out this line if you want to
    # download all files OR make download.all = TRUE
    if (isFALSE(download.all)) {
      filenames <- filenames[c(grep("NHDSnapshot_",filenames),
                               grep("NHDPlusAttributes_",filenames),
                               grep("WBDSnapshot_",filenames))]
      #print(filenames)
      
    }
    
    
    for(filenumber in 1:length(filenames)) {
      tf <- tempfile() #paste0("./tmpfile",filenumber,".7z") # default name of the archive
      on.exit(unlink(tf))
      # curl.download(paste0(folderpath[i],filenames[filenumber]),tf,mode = "wb")
      download.file(paste0(folderpath[i],filenames[filenumber]),tf,mode = "wb",quiet = TRUE)
      
      archive_extract(archive(tf),out.dir)  
         
    }
    
    dataset[[i]] <-  paste0(out.dir, str_remove(folderpath[i],mainurl)) 
    
    }
  
  names(dataset) <- make_listNames(folderpath)
  
  return(dataset)
  
}

make_listNames <- function(x){
  index <- tail(str_locate_all(x,"/")[[1]][,1],2) 
  xname <- str_sub(x,index[1]+1,index[2]-1)
  return(xname)
}

