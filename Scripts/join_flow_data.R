join_flow_data <- function(nid.df,site.df,Flowdata,colname){

   
   # check for repeated ComID in both data 
   chk1 <- plyr::count(nid.df,vars=colname)
   chk2 <- plyr::count(site.df,vars=colname)
   
   chkrows <- dplyr::full_join(chk1,chk2,by=colname)
   
   #chkrows <- dplyr::union(chk1,chk2)
   names(chkrows) <- c('COMID','damsCount','gageCount')
   class(chkrows$COMID) <- "integer"
  
   # extract Flowdata
   tmp <- Flowdata %>%  dplyr::mutate(PointType = NA) 
   # %>%  select(-FDate,-RESOLUTION,-Shape_Leng,-ENABLED)
   class(tmp$COMID) <- "integer" 
   tmp <- dplyr::left_join(tmp,chkrows,by='COMID')  %>% dplyr::select(-ends_with(".y")) %>% 
     dplyr::rename_at(vars(ends_with(".x")),funs(str_remove(.,".x"))) 
   
   tmp$PointType[is.na(tmp$damsCount)& !is.na(tmp$gageCount)] <- 'gage'
   tmp$PointType[!is.na(tmp$damsCount)& is.na(tmp$gageCount)] <- 'dam'
   tmp$PointType[!is.na(tmp$damsCount)& !is.na(tmp$gageCount)] <- 'both'
   
   tmp <- tmp %>% dplyr::rename_all(str_to_upper)
  
    print( table(unlist(tmp$POINTTYPE)) ) 
   
    
   return(tmp)
}