

#' Function to add data from excel to an existing table in a layer of a geopackage
#'
#' @param geopackage The complete path to the geopackage where the data should be added.
#' @param data The path to the excel file from which the data is added to the geopackage file. The excel should contain all columns of the table of the geopackage in the same order. Additionally, there should be columns with the x and y coordinates ("X" and "Y") to generate the point geometry. 
#' @param layer The layer of the geopackage, in which the data should be added to the datatable.
#' @param crs The coordinate system to generate the geometry. Defaults to 2056.


#' 
#' @return Update geopackage
#'
#' @author David Frei, \email{david.frei@fornat.com}
#'
#'
#' @import readxl
#' @import gpkg
#' @import sf
#' @import tibble
#' @export








add_to_geopackage<-function(geopackage,data,layer,crs=2506){
  
  #make geometry for the data frame
  for(i in 1:nrow(df)){
    
    #read data
    df <- as.data.frame(read_excel(data))
    #take x and y coordinates of every row of the data frame and make a geometry
    geometry <- st_sfc(st_point(cbind(as.numeric(df[i,]$X), as.numeric(df[i,]$Y))))
    
    #delete x and y columns (not needed anymore)
    df <- df[, !(names(df) %in% c("X","Y"))]
    
    #add geometry to the data that should be added to the geopackage
    insert<-st_sf(df[i,], geometry = geometry)
    st_crs(insert) <- st_crs(crs)
    
    #add to data with geometry to geopaackage
    st_write(insert,geopackage,layer = layer,append=T,driver="GPKG",quiet=T)
    
  }
}


