#' @param CAT.ID 
#'
#' @title Function to create NWIS sites depending on HUC2 region
#' @description Downloads NWIS streamflow sites that has daily discharge in cfs 
#' This function does NOT download streamflow data. It only downloads spatial 
#' locations of the stations. If you need HCDN stations, you need to filter the site list. 
#' 
#' @import dplyr  
#' @import stringr
#' @import dataRetrieval
#' 
#' @return sites
#'
#' @examples
#' \donotrun{
#' build_sitelist("06") # watershed number at HUC02 level 
#'	}
#' 
build_sitelist <- function(CAT.ID) {
  
  
  sites_tmp <- dataRetrieval::whatNWISsites(huc = CAT.ID, # Catchment ID at HUC 02 level 
                             siteType = 'ST', # Streams only 
                             parameterCd = "00060", #discharge in cubic  feet per second 
                             hasDataTypeCd = 'dv') # daily data 
  
  sites_basin <- sites_tmp %>% tidyr::drop_na() 
  
  site_list <- as.list(sites_basin$site_no)
   
  atr_base <- lapply(site_list,dataRetrieval::readNWISrating)
  
  attr_df <- lapply(atr_base,attr,"siteInfo")
  nlen <- unlist(lapply(attr_df,function(x) length(x) > 0 ))
  
  df <- attr_df[nlen]
  nrows <- length(df)
  
  df. <- data.frame(matrix(unlist(df), nrow=nrows, byrow=T),
                    stringsAsFactors=FALSE)
  
  colnames(df.) <- colnames(df[[1]])
  
  
  sites <- sites_basin %>% 
    dplyr::left_join(sites_basin,df., by= 'site_no') %>% 
    dplyr::select(-ends_with(".y")) %>% 
    dplyr::rename_at(vars(ends_with(".x")),funs(str_remove(.,".x"))) %>% 
    dplyr::rename(Latitude = dec_lat_va,Longitude = dec_long_va) 
  
  return(sites)

  
}




#' @title Function to clean up character vectors 
#' @param x 
#'
#' @return
#' @export
#'
#' @noRd
string_prep <- function(x){
  # x is a string vector 
  # remove X from if present 
  x. <- gsub("X","",x)
  
  # add 0 in the first digit if not present 
  ic <- regexpr("0",x.)
  ic <- which(ic != 1)
  out <- x.
  out[ic] <- paste0("0",x.[ic])
  
  # USGS site numbers must be no longer than 15 numeric digits 
  ic <- which(nchar(out) > 15) 
  out[ic] <- substring(out[ic],2)  # remove first character 
  return(out)
}


#' Check availability of streamflow data by site number
#'
#' @param site_list 
#'
#' @return
#' @export
#'
#' @noRd 
check_streamflow <- function(site_list){
  # site_list is a string vector of site numbers only 
  av_streamflow <- NULL
  for(i in 1:length(site_list)){
    print(paste0("Checking data availability for site ", i, " out of ",length(site_list)))
    nwisdata <- whatNWISdata(sites=site_list[i],
                             parameterCd="00060",
                             outputDataTypeCd = 'dv',
                             statCd="00003") 
    
    if(length(nwisdata)>0){
      streamflow <- nwisdata %>%
        select(c("site_no","data_type_cd","parm_cd","stat_cd",
                 "begin_date","end_date","count_nu")) %>%
        mutate(date_diff = as.numeric(as.Date(end_date,"%Y-%m-%d") - as.Date(begin_date,"%Y-%m-%d"))+1,
          URL=attr(nwisdata,"url"))
    }
    
    av_streamflow=rbind(av_streamflow,streamflow)
  }
  
  return(av_streamflow)
  
}



#' Downloads daily streamflow data and saves them in a new directory
#'
#' @param site_list 
#' @param mainDir 
#' @param subDir 
#' @param writeToFile 
#'
#' @return
#' @export
#'
#' @noRd
add_streamflow <- function(site_list,mainDir = getwd(),subDir = 'USGSdata',writeToFile = TRUE) {
  # site_list is a string vector of site numbers only 
  
  if (isTRUE(writeToFile)) {
    dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
    
  }
  
  dv_streamflow <- NULL
  for(i in 1:length(site_list)){
    print(paste0("Downloading data for site ", i, " out of ",length(site_list)))
    nwisdata <- readNWISdv(siteNumbers=site_list[i],
                           parameterCd="00060",
                           statCd="00003") 
    
    if (isTRUE(writeToFile)) {write.csv(nwisdata, paste0(subDir,'/gage.', site_list[i], '.csv'))}
    
    if(length(nwisdata)>0){
      streamflow <- nwisdata %>%
        select(2:4) %>%
        set_names(c("site_no","date","Q")) %>%
        mutate(year=year(date),
               month=month(date))
    }
    
    dv_streamflow=rbind(dv_streamflow,streamflow)
  }
  
  return(dv_streamflow)
  
}



#' Data arrangement and removal of missing data 
#'
#' @param dv 
#' @param site_info 
#'
#' @return
#' @export
#'
#' @noRd
prepare_streamflow <- function(dv,site_info){
  
 dv_clean <- dv %>%
    filter(Q != -999999) %>%
    group_by(site_no)  
 #%>%
 #   spread(site_no,Q)
  
  return(dv_clean)
}

