#' Nearest_NHDflowline
#'
#' @param pts. # point shapefile regular or sf
#' @param lines.  # lines shapefile regular or sf 
#' @param tol Minimum distance tolerance in Meter
#'
#' @import dplyr
#' @import sf
#' @import stringr
#' @import tidyr
#' @import purrr
#' @import maptools
#' @return
#' @export
#'
#' @examples
nearest_NHDflowline <- function(pts.,lines.,tol = 1000,data.sources = "./data/"){
  
  pts. <- sf::st_as_sf(pts.)
  lines. <- sf::st_as_sf(lines.)
  
  if(!isTRUE("IDS" %in% names(pts.))){
   cat("Point data does not have IDS field.")
   cat("Available column names : ",'\n',names(pts.))  
   varname <- readline("Enter a column name to create IDS (without quotes):")
  
   pts. <- pts. %>% mutate(IDS = NULL)
   pts.[['IDS']] <- pts.[[varname]]
   cat("New column IDS is created using ", varname)
  }
 
  
 # Stage 0: adjust projections and save geometry 
  # Output 
  sfc <- sf::st_geometry(pts.)
  sfc.df <- matrix(unlist(lapply(sfc, function(x) as.numeric(x))),
                   ncol=2,byrow = TRUE)
  colnames(sfc.df) <- c("LONGITUDE","LATITUDE")
  
  utm_zones <- table(unlist(lapply(sfc.df[,1],wgs_to_utm)))
  cat("UTM zone of the point data : ",'\n')
  print(utm_zones)
  
  utm_ <- names(which.max(utm_zones)) # this may be modified for larger basins
  cat("Selecting zone number : ",utm_,'\n')
  crs_proj <-  paste0("+proj=utm +zone=",utm_," ellps=WGS84")
  
  pts.proj <- sf::st_transform(pts.,crs = crs_proj) %>% dplyr::rename_all(stringr::str_to_upper)
  lines.proj <- sf::st_transform(lines.,crs = crs_proj) %>% dplyr::rename_all(stringr::str_to_upper)
  
  # Step 1 : Link points to flowlines by COMID -----------
  
  # Tentative nearest ComID 
  nearestFeature <- sf::st_nearest_feature(st_zm(pts.proj),st_zm(lines.proj))
  
  # arrange by nearest ComID
  lines_nearest <- lines.proj[nearestFeature,]
  
  pts.proj <- dplyr::mutate(pts.proj, TMP.COMID = lines_nearest$COMID)
  
  # Initial string comparison : basic match  
  # # #compare strings pts.proj$info and lines_nearest$GNIS_NAME
  # # Note: flag =  0 (good string match), 1 ( no string match) ,
  # # 2 (Missing GNIS_NAME of stream)
  
  a <- as.character(pts.proj$INFO) # points names 
  b <- as.character(lines_nearest$GNIS_NAME) # nhdflowline names 
  
 
  # remove some patterns 
  patterns <- c("RIVER|RV|CREEK|CR|NEAR|\tNR\t|\tBL\t|BELOW|ABOVE|AT|[[:punct:]]|[[:digit:]]|[[:blank:]]|[[:space:]]")
  a. <- unlist(lapply(a, function(s) stringr::str_remove_all(str_to_upper(s),patterns)))
  b. <- unlist(lapply(b,function(s) stringr::str_trim(str_remove_all(str_to_upper(s),patterns ))))
  
  flag <- numeric()
 
  
  # basic string detection : checks for perfect match 
  for(i in 1:length(a.)){
    if(is.na(b.[[i]])) { 
      flag[i] <- 2 # missing GNIS NAME
    } else if(isTRUE(stringr::str_detect(string = a.[i],pattern = b.[i])) | 
              isTRUE(stringr::str_detect(string = b.[i],pattern = a.[i]))) { 
      flag[i] <- 0 # good match
    } else {
      flag[i] <- 1 # no match
    }
  }
  
  pts.proj <- dplyr::mutate(pts.proj, FLAG = flag)
# --------
  #  # print initial diagnostics : 
  # print(paste0("River and point names perfectly matched for ", length(flag[flag == 0]), " points out of ",
  #              length(a.), " (",round(100*( length(flag[flag == 0])/length(a.)),0) , " %)"))
  # print(paste0("Additional checks needed for ", length(flag[flag > 0]), " points out of ",length(a.)))
  # print(paste0("Check distances for missing River name for ", length(flag[flag == 2]), " points out of ",
  #              length(a.), " (",round(100*( length(flag[flag == 2])/length(a.)),0) , " %)"))
  # print(paste0("Names need to be approximately matched for ",length(flag[flag == 1]), " points out of ",
  #              length(a.), " (",round(100*( length(flag[flag == 1])/length(a.)),0) , " %)"))
  # 
 #------------- 
   
  #-- Step 2 : create buffer around points and check overlapping lines  ---------
  # Calculate distance of points from lines within buffer zone 
  # Need to Handle points with FLAG = 1 OR 2 
   
   # if object doesnt exist, save it ! this is of HUGE size
 

    pts_with_buffer <- sf::st_buffer(pts.proj,tol) 
    
    lines_within_pts_buffer <- st_intersection( pts_with_buffer, lines.proj)
    
  
     
    if(dim(pts.proj)[1] > length(unique(lines_within_pts_buffer$IDS))){
      message("Not all points have flowlines within the buffer distance.")}
    
    # output 
    list_lines_within_pts_buffer <- lines_within_pts_buffer %>% 
      sf::st_drop_geometry() %>% 
      dplyr::select(IDS,INFO,TMP.COMID,FLAG,COMID, GNIS_NAME,LEVELPATHI) %>% 
      dplyr:: mutate(TEST.COMID = COMID) %>% 
      dplyr::select(-COMID) %>% 
      dplyr::group_by(IDS) %>% 
      dplyr::group_split() 
      
    #Output 
    num_lines_points_buffer <- sapply(list_lines_within_pts_buffer,
                                      function(x) dim(x)[1])
    # Size of this will depend on the tolerance (buffer distance)
    # output 
    points_not_considered <- pts.proj %>% 
             filter(!IDS %in% lines_within_pts_buffer$IDS)
    
    # Output 
    list_dist_buffer <- lapply(list_lines_within_pts_buffer,
                               st_distance_buffer,
                               pts.proj, lines.proj)
   

  #### step 3 : Compare multiple lines within buffer zone ------------------
  # Compare names of the rivers reaches that are within tolerance   
 
  list.pts.nrst.line <- lapply(list_dist_buffer,link_line_to_point )
     
  df.pts.nrst.line <- do.call(rbind,list.pts.nrst.line) 
 
 
  
  pts_linked <- pts.proj %>% sf::st_drop_geometry() %>% 
                dplyr::right_join( df.pts.nrst.line,by = "IDS") %>% 
                dplyr::select(-ends_with(".y")) %>% 
                dplyr::rename_at(vars(ends_with(".x")),funs(str_remove(.,".x")))  %>% 
                dplyr::mutate(NEAREST_LINE_ID = COMID)  
    
  
  #----------------------------------------------------------------
   # pts_ <- pts_linked %>% 
   #   dplyr::mutate(COMID = COMID.test)  %>% 
   #   left_join(pts.proj,by = "IDS") %>% 
   #   dplyr::select(-ends_with(".y")) %>% 
   #   dplyr::rename_at(vars(ends_with(".x")),funs(str_remove(.,".x"))) %>% 
   #   sf::st_as_sf() %>% 
   #   purrr::transpose()
   # 
   #  lines_ <- lines_nearest 
    #%>% 
   #   dplyr::select(-ends_with(".y")) %>% 
   #   dplyr::rename_at(vars(ends_with(".x")),funs(str_remove(.,".x"))) %>% 
   #   sf::st_as_sf() %>% 
   #   purrr::transpose()
   
   # # Snapping [call it separately]
   #   pts_snapped_list <- lapply(pts_,callSnapPointsToLines,lines_,utm_)
   # 
   #  pts_snapped <- lapply(pts_snapped_list,sf::st_as_sf)
   # 
   #  pts_snapped_ <- bind_rows(pts_snapped) %>% 
   #                  dplyr::mutate(COMID = nearest_line_id) 
   #  
   #  pts_linked <- pts_linked %>% 
   #    dplyr::mutate(COMID = COMID.test)
   # 
   #   pts_snapped_ <- bind_cols(pts_linked,pts_snapped_) %>% 
   #                  dplyr::select(-ends_with(".y")) %>% 
   #                  dplyr::rename_at(vars(ends_with(".x")),
   #                                   funs(str_remove(.,".x"))) 
   
    
   #---------- output -------------
   
   return(list(pts.linked = pts_linked,
               Input.Data = list(p = pts.proj, 
                                 l = lines_nearest,
                                 tol.METER = tol,
                                 lon_lat = sfc.df, 
                                 p_str = a., 
                                 l_str = b. ),
               points_leftout = points_not_considered,
               num_lines_points_buffer = num_lines_points_buffer, 
               lines_within_pts_buffer = list_lines_within_pts_buffer, 
               list_lines_dist_buffer = list_dist_buffer, 
               crs_proj = crs_proj)) 
 
}




#' @title Approximate String matching 
#'
#' @param x
#' @param y
#'
#' @import dplyr
#' @import stringr
#' @import stringdist
#'
#' @return
#' @examples
strmatch <- function(x,y){
  # Make sure these patterns are removed 
  patterns <- c("RIVER|RV|CREEK|CR|NEAR|\tNR\t|\tBL\t|BELOW|ABOVE|AT|[[:punct:]]|[[:digit:]]|[[:blank:]]|[[:space:]]")
  x. <- unlist(lapply(x,function(s) stringr::str_trim(str_remove_all(str_to_upper(s),patterns ))))
  y. <- unlist(lapply(y,function(s) stringr::str_trim(str_remove_all(str_to_upper(s),patterns ))))
  
  
  # Edit based methods : osa, lv, dl, lcs 
  # q-gram based methods :  qgram, cosine, jaccard 
  # Reference: MPJ van der Loo (2014) The stringdist package for approximate
  # string matching. The R Journal 6(1) 111-122.
  
  distance_methods <- c('osa','lv','dl','lcs','qgram','jaccard')
  
  distance_by_methods<-list()
  for(m in 1:length(distance_methods))
  {
    dist.vec <- c()
    for(i in 1:length(x.)) {
      
      dist.vec[i]<-stringdist::stringdist(x.[i], y.[i],
                                               method =  distance_methods[m])      
    }
    distance_by_methods[[ distance_methods[m]]]<-dist.vec 
  }
  
  min.distance_by_methods <- sapply(distance_by_methods, base::which.min) 
  
  # choose most freq column number 
  min.dist.id <- as.numeric(names(which.max(table(min.distance_by_methods))))
  
  
  # return position of the best match and all distances 
  return( list(nrst_col_id = min.dist.id, 
               dist_methods = distance_by_methods)) 

  }


#' Links points to nearest line by comparing distance AND names 
#'
#' @param px point buffer 
#'
#' @return
#' @noRd 
link_line_to_point <- function(px){
  
  px <- as.data.frame(px)
  px$d.COMID <- as.numeric(px$d.COMID)
  #px
  # Create new columns in px 
  # names(px)
 # "IDS" "COMID"  "GNIS_NAME" "INFO" "TMP.COMID" "FLAG" "d.COMID"  
  
  
  
  if((px$FLAG > 0) & (dim(px)[1] > 1) & !(all(is.na(px$GNIS_NAME)))) { 
   # Do fuzzy string matching and alert for VFLAG 
    px.d <-  strmatch(px$INFO, px$GNIS_NAME) 
    new.comid <- px$TEST.COMID[px.d$nrst_col_id]
    
     if(new.comid == unique(px$TMP.COMID)){
       px <- px %>% dplyr::mutate(COMID = TMP.COMID, 
                                  VFLAG = FLAG) # 0 no visual check needed 
     } else{ # Comapre level path ids 
       tmp.levelpathid <- px$LEVELPATHI[which(px$TMP.COMID == px$TEST.COMID)]
       test.levelpathid <- px$LEVELPATHI[px.d$nrst_col_id]
       
       if(tmp.levelpathid == test.levelpathid){
         # Don't change tmp.comid 
         # visual check is optional. Choose line with lower d.comid 
         px <- px %>% dplyr::mutate(COMID = TMP.COMID, 
                                    VFLAG = 1) # 1 optional visual check  
         
       } else { 
         # string match is suggesting a flowline with different LevelPathID 
        
         if ( px$d.COMID[px.d$nrst_col_id] <= (2*min(px$d.COMID)) ) {
           # Randomly selecting twice the nearest distance 
           # Change COMID 
           # Visual check suggested as selecting new flowline   
           px <- px %>% dplyr::mutate(COMID = new.comid, 
                                      VFLAG = 2) # 2 suggested visual check 
         } else if ( px$d.COMID[px.d$nrst_col_id] > (2*min(px$d.COMID)) ) {
           # Dont change COMID 
           px <- px %>% dplyr::mutate(COMID = TMP.COMID, 
                                      VFLAG = 1) # 1 optional visual check  
         }
       }
     }
    
  } else if ((px$FLAG == 0)){
    # Keep the TMP.COMID as the selected COMID and VFLAG = 0 
    px <- px %>% dplyr::mutate(COMID = TMP.COMID, 
                        VFLAG = FLAG) # 0 no visual check needed 
  } else if ((px$FLAG > 0) & (dim(px)[1] == 1)){
    # No other line within the buffer distance 
    # Keep the TMP.COMID as the selected COMID and VFLAG = 0 
    px <- px %>% dplyr::mutate(COMID = TMP.COMID, 
                               VFLAG = 0) # 0 no visual check needed 
  } else if(all(is.na(px$GNIS_NAME))){
    # No way to match strings 
    # Keep the TMP.COMID as the selected COMID and VFLAG = 0 
    row.id <- which.min(px$d.COMID)
    px <- px %>% dplyr::mutate(COMID = px$TEST.COMID[row.id], 
                               VFLAG = 1) # 1 optional visual check    
   
  }
  
  
  # Keep the chosen COMID row only 
   row.selected <- which(px$TEST.COMID == px$COMID) 
   px <- px[row.selected,]
  
  return(px)
  
}

#' Title
#'
#' @param x 
#' @param dist_list 
#' @param db 
#'
#' @return
#' @export
#'
#' @examples
distance_FromHeadWater <- function(x = NULL,dist_list,db)
{
 
  # x : vector of point ids 
  # dist_list : distance data frame with IDS, ComID, distFromNode, distToNode
  # db : NHDFlowplus data
  
  
  dist_list <- as.data.frame(dist_list) %>% dplyr::rename_all(stringr::str_to_upper)
  db <- db %>% dplyr::rename_all(stringr::str_to_upper) 
  
  if(!isTRUE("IDS" %in% names(dist_list))){
    cat("Point data does not have IDS field.")
    cat("Available column names : ",'\n',names(dist_list))  
    varname <- readline("Enter a column name to create IDS (without quotes):")
    
    dist_list <- dist_list %>% mutate(IDS = NULL)
    dist_list[['IDS']] <- dist_list[[varname]]
    cat("New column IDS is created using ", varname)
  } 
  
  if (!is.null(x)){
    d_list <- dist_list %>% dplyr::filter(IDS %in% x) %>% 
      select(IDS,ComID,distToNode) # distToNode in Meters 
  } else {
    d_list <- dist_list %>% dplyr::select(IDS, everything())
  }
    
  
  db. <- db %>% filter(COMID %in% d_list$NEAREST_LINE_ID) %>% select(COMID,ARBOLATESU)
  
  out <- left_join(d_list,db.,by='COMID') %>% 
              dplyr::select(-ends_with(".y")) %>% 
             dplyr::rename_at(vars(ends_with(".x")),funs(str_remove(.,".x"))) 
 
   #  HEADWATER DISTANCE in KM 
  out <- out %>% dplyr::mutate(DIST_FROMHWKM =  ifelse(ARBOLATESU - 0.001*DISTTONODE > 0,
                                                ARBOLATESU - 0.001*DISTTONODE,0)) %>% 
           dplyr:: select(IDS,everything())   
  
  
  return(out)
}



#' Title
#'
#' @param points.shp 
#' @param lines.shp 
#'
#' @return
#' @export
#'
#' @examples
distance_fromNodes <- function(pts.,lines.){
  
  pts. <- sf::st_as_sf(pts.)
  lines. <- sf::st_as_sf(lines.)
  
  if(!isTRUE("IDS" %in% names(pts.))){
    cat("Point data does not have IDS field.")
    cat("Available column names : ",'\n',names(pts.))  
    varname <- readline("Enter a column name to create IDS (without quotes):")
    
    pts. <- pts. %>% mutate(IDS = NULL)
    pts.[['IDS']] <- pts.[[varname]]
    cat("New column IDS is created using ", varname)
  }
  
  
  # Stage 0: adjust projections and save geometry 
  # Output 
  sfc <- sf::st_geometry(pts.)
  sfc.df <- matrix(unlist(lapply(sfc, function(x) as.numeric(x))),
                   ncol=2,byrow = TRUE)
  colnames(sfc.df) <- c("LONGITUDE","LATITUDE")
  
  utm_zones <- table(unlist(lapply(sfc.df[,1],wgs_to_utm)))
  cat("UTM zone of the point data : ",'\n')
  print(utm_zones)
  
  utm_ <- names(which.max(utm_zones)) # this may be modified for larger basins
  cat("Selecting zone number : ",utm_,'\n')
  crs_proj <-  paste0("+proj=utm +zone=",utm_," ellps=WGS84")
  
  pts.proj <- sf::st_transform(pts.,crs = crs_proj) %>% dplyr::rename_all(stringr::str_to_upper)
  lines.proj <- sf::st_transform(lines.,crs = crs_proj) %>% dplyr::rename_all(stringr::str_to_upper)
  
 
  if (all(is.na(match(names(pts.proj), c("NEAREST_LINE_ID"))))){
    stop("First identify nearest NHDFlowline using function : nearest_NHDflowline")
  } else {
    
    lines_ <- lines.proj %>%  
              dplyr::filter(COMID %in% pts.proj$NEAREST_LINE_ID)  %>% 
              sf::st_as_sf() 
    
    
    pts_ <- pts.proj %>% sf::st_as_sf() %>% purrr::transpose()
    
    
    
#  #  Snap points to previously identified nearest lines : 
     
     pts_snapped_list <- lapply(pts_,callSnapPointsToLines,lines_,utm_)
     
     # Call length to point for each element 
     # List of spatialPointsDataFrame 
     pts_snapped_distToNodes <- lapply( pts_snapped_list, callLengthToPoints, lines_)  
     
    # List of sf 
     pts_snapped <- lapply(pts_snapped_distToNodes,sf::st_as_sf) 
     
     pts_snapped_ <- dplyr::bind_rows(pts_snapped) %>% 
                     dplyr::rename_all(str_to_upper) %>% 
                     dplyr::mutate(IDS = unlist(pts.$IDS))
     
     
    
  }
  
  return( pts_snapped_) 

}


#' wgs_to_utm
#'
#' @param x
#' @import sf
#'
#' @return
#'
#' @noRD
wgs_to_utm <- function(x){
  # for CONUS only : UTM zone : 10 to 19
  # Can be improved ? 
  if(max(range(abs(x))) > 180 ) x = abs(x) - 180
  
  x = abs(x)
  
  if(x >= 120){
    zone_num <- 10
  } else if(x < 120 & x >= 114) {
    zone_num <- 11
  } else if (x < 114 & x >= 108) {
    zone_num <- 12
  } else if (x < 108 & x >= 102){
    zone_num <- 13
  }else if (x < 102 & x >= 96){
    zone_num <- 14
  }else if (x < 96 & x >= 90){
    zone_num <- 15
  }else if (x < 90 & x >= 84){
    zone_num <- 16
  }else if (x < 84 & x >= 78){
    zone_num <- 17
  }else if (x < 78 & x >= 72){
    zone_num <- 18
  }else if (x < 72 & x >= 66){
    zone_num <- 19
  }else if (x < 66 ){
    zone_num <- 20
  }
  
  
  
  # proj_ <-  paste0("+proj=utm +zone=",zone_num," ellps=WGS84")
  
  return(zone_num)
  
}



#' @title Function to join list of dataframes row-wise into a combined dataframe 
#' @param ... 
#' @return the joined dataframe
#' @noRd
list.bind.rows <- function(...){
  
dplyr::bind_rows(...)
}



#' @title Function to join list of dataframes column-wise  into a combined dataframe 
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @noRd
list.bind.cols <- function(...){
 dplyr::bind_cols(...)
}




#' Title
#'
#' @param p 
#' @param l 
#' @param z 
#'
#' @return
#' @export
#'
#' @noRd
callSnapPointsToLines <- function(p,l,z){
  
  psf <- list_to_sf(p)  
  st_crs(psf) <- st_crs(l) <- paste0("+proj=utm +zone=",z," ellps=WGS84")
  
  lsf <- l %>% filter(COMID == psf$COMID) %>% select(everything()) 
  
  psp <- sf::as_Spatial(psf)
  lsp <- sf::as_Spatial(lsf)
  
 y <-  snapPointsToLines(psp,lsp,idField = "COMID")  
   
  return(y)
  
}



#' Title
#'
#' @param p 
#' @param l 
#'
#' @return
#' @export
#'
#' @examples
callLengthToPoints <- function(p,l){
  thisLine <- l %>% 
              filter(COMID == p@data$nearest_line_id) %>% 
              select(everything()) 
   
  
  lsp <- sf::as_Spatial(thisLine)
  
  d <- lengthToPoint(lsp,p) # in meter
  p@data$distFromNode <- d
  p@data$distToNode <- gLength(spgeom = lsp) - d # in meter 
  
  
  
  p@data <- p@data %>% dplyr::mutate(ComID = nearest_line_id) %>%
            dplyr::select(everything()) 

  return(p)
  
}




#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @noRd
list_to_sf <- function(x){
    
   names(x)  <- stringr::str_to_lower(names(x)) 
  
    x_geo <- x$geometry 
    x$geometry <- NULL
    # data frame 
    xdf <- as.data.frame(x, stringsAsFactors = FALSE) %>% mutate(lon = x_geo[1],
                                                                 lat = x_geo[2] )
    
    xsf <- sf::st_as_sf(xdf,coords = c("lon","lat"))%>% 
           dplyr::rename_all(stringr::str_to_upper)
    
    
  return(xsf)
  
}




#' Title
#'
#' @param x 
#' @param pts_sf 
#' @param lines_sf 
#'
#' @return
#' @export
#'
#' @examples
st_distance_buffer <- function(x, pts_sf,lines_sf){
  
  pts_sf <- pts_sf %>% dplyr::filter(IDS %in% x$IDS)
  lines_sf <- lines_sf %>% dplyr::filter(COMID %in% x$TEST.COMID)
  
  d <- sf::st_distance(st_zm(pts_sf), st_zm(lines_sf), 
                   by_element = FALSE)
  
  d <- as.numeric(d) 
  
  x <- x %>% dplyr::mutate(d.COMID = d) %>% dplyr::select(everything())
  
  # distance : dim(d) : length(x) by length(y)
  # d <- st_distance(st_zm(pts.proj), st_zm(lines_nearest),
  #                  by_element = FALSE) 
  # ignored if st_is_longlat(x) is FALSE; otherwise, if set to a positive value,
  # the first distance smaller than tolerance will be returned, and true
  # distance may be smaller; this may speed up computation. In meters, or a
  # units object convertible to meters
  return(x) 
} 


#' Title
#'
#' @param spatialLinesObj 
#' @param intersectionPoint 
#' @param buffer 
#'
#' @return
#' @export
#'
#' @examples
lengthToPoint <- function(spatialLinesObj, intersectionPoint, buffer = 0.01){
  
  # Following : https://gis.stackexchange.com/questions/209254/calculate-distance-of-points-spatialpoints-object-along-a-path-spatialline
  
  
  intersectionPointCoordinates = intersectionPoint@coords
  bufferedIntersectionPoint = gBuffer(spgeom = intersectionPoint, width = buffer)
  # return 0 if there is no intersection at all
  if(!gIntersects(spatialLinesObj, bufferedIntersectionPoint)){
      print("The line does not intersect with the (buffered) point!")
    return(0)
  }
  lineCoordinates = spatialLinesObj@lines[[1]]@Lines[[1]]@coords
  numOfCoordinates = length(lineCoordinates[,1])
  calculatedLength = 0
  # split line into segments
  for(i in 2:numOfCoordinates){
    # create new spatiallines for the current segment and check if point is intersecting
    currentLine = SpatialLines(LinesList = list(Lines(slinelist = list(Line(coords = lineCoordinates[(i-1):i,])), ID = "1")), spatialLinesObj@proj4string)
    # no intersection
    if(!gIntersects(currentLine, bufferedIntersectionPoint)){
      calculatedLength = calculatedLength + gLength(spgeom = currentLine)
      # intersection
    } else {
      # create line from start of current segment to intersection point
      coordinates = matrix(data = c(lineCoordinates[i-1,], intersectionPointCoordinates), nrow = 2, byrow = T)
      lastLine = SpatialLines(LinesList = list(Lines(slinelist = list(Line(coords = coordinates)), ID = "1")), spatialLinesObj@proj4string)
      calculatedLength = calculatedLength + gLength(spgeom = lastLine)
      #   print(paste("Found point on line segment", (i-1), "! Length from start point: ", calculatedLength))
      return(calculatedLength) # in meter
    }
  }
}


keep.unique.cols <- function(x){
  names.vec <- stringr::str_to_upper(names(x))
  dt.names <- data.table::as.data.table(names.vec)[, list(list(.I)), by = names.vec]
  dt.names.id <- sapply(dt.names$V1, function(x) x[1])
  x <- x[,dt.names.id]
  return(x)
}