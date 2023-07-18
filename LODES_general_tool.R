
### Installing and uploading the proper packages in R.

# install.packages("dplyr")
# install.packages("lehdr")
# install.packages("mapview")
# install.packages("osrm")
# install.packages("sf")
# install.packages("tigris")

library(dplyr)
library(lehdr)
library(mapview)
library(osrm)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

### Uploading the LODES dataset for California.

ca_lodes <- grab_lodes(state = "ca", year = c(2019), lodes_type = "od", job_type = "JT00", 
           segment = "S000", state_part = "main", agg_geo = "tract") # agg_geo = "bg"

nrow(ca_lodes)

# https://cran.r-project.org/web/packages/lehdr/readme/README.html
# https://cran.r-project.org/web/packages/lehdr/vignettes/getting_started.html

### Uploading the shapefiles for all counties and census tracts in California.

ca_counties <- counties(state = "CA", cb = FALSE, year = NULL)
ca_tracts <- tracts(state = "CA", cb = FALSE, year = NULL)
ca_bgs <- block_groups(state = "CA", cb = FALSE, year = NULL)

ca_counties_geoid <- ca_counties %>% dplyr::select(c("GEOID", "geometry"))
ca_tracts_geoid <- ca_tracts %>% dplyr::select(c("GEOID", "geometry"))
ca_bgs_geoid <- ca_bgs %>% dplyr::select(c("GEOID", "geometry"))

### Associating the geometry files with the California LODES dataset.

# ca_lodes_geo <- ca_lodes %>% left_join(ca_tracts_geoid, by = c('w_tract' = 'GEOID')) %>% st_as_sf()
# ca_lodes_geo <- ca_lodes %>% left_join(ca_bgs_geoid, by = c('h_tract' = 'GEOID')) %>% st_as_sf()

### Saving and loading data.

# save.image("save.RData")
# load("save.RData")

### Choosing the block groups of interest, based on the boundary of the county of interest.

ca_counties_geoid_interest <- ca_counties_geoid[ca_counties$NAME == "San Francisco", ]
ca_tracts_geo_county_bound <- st_intersection(ca_tracts_geoid, ca_counties_geoid_interest)
ca_tracts_geo_county_bound$GEOID.1 <- NULL

### Reducing the LODES data to those associated with the homes in the county of interest.

h_tract_county_bound <- c(ca_lodes$h_tract %in% ca_tracts_geo_county_bound$GEOID)
ca_lodes_county_bound <- ca_lodes[h_tract_county_bound, ]

### Joining the load data for the country of interest with shapefile data for each work census tract.

ca_lodes_county_bound_geo <- ca_lodes_county_bound %>% left_join(ca_tracts_geoid, by = c('w_tract' = 'GEOID')) %>% st_as_sf()

### Creating an array of unique census tracts for the census tracts in the California LODES database.

h_tract_unique <- unique(ca_lodes_county_bound_geo$h_tract)

### Creating data frames for a home census tract of interest and the corresponding work census tracts.

h_tract_unique_iter <- h_tract_unique[1]

ca_tracts_county_bound_tract <- ca_tracts[ca_tracts$GEOID == h_tract_unique_iter, ]
ca_lodes_county_bound_geo_tract <- ca_lodes_county_bound_geo[(ca_lodes_county_bound_geo$h_tract == h_tract_unique_iter), ]

### Mapping the single home census tract and the corresponding work census tracts.

m <- mapview(ca_lodes_county_bound_geo_tract, col.regions = "red") + mapview(ca_tracts_county_bound_tract, col.regions = "blue")
m

### Creating an origin-destination distance matrix for all block groups within California.

bgs_table <- data.frame(matrix(nrow = 0, ncol = 3, data = NA))
colnames(bgs_table) <- c("h_bgs", "w_bgs", "dist (km)")

ca_bgs_osrm <- ca_bgs
ca_bgs_osrm$geometry <- NULL

for (iter_ in 1:nrow(ca_bgs_osrm)){
  
  src_iter <- ca_bgs_osrm[iter_, ] %>% dplyr::select(c("GEOID", "INTPTLON", "INTPTLAT"))
  dst_iter <- ca_bgs_osrm %>% dplyr::select(c("GEOID", "INTPTLON", "INTPTLAT"))
  
  print(iter_)
  
  for(iter2_ in 1:233){
  
    dst_iter <- ca_bgs_osrm[c( (1+(iter2_-1)*100):(iter2_*100) ),] %>% dplyr::select(c("GEOID", "INTPTLON", "INTPTLAT"))
  
    osrmTable_iter <- osrmTable(src = src_iter, dst = dst_iter, measure = "distance")$distances/1000
    # osrmTable_iter <- osrmTable(src = src_iter, dst = dst_iter)$durations
    
    osrmTable_iter_bind <- cbind(data.frame(src_iter$GEOID), data.frame(dst_iter$GEOID), t(osrmTable_iter))
    
    rownames(osrmTable_iter_bind) <- NULL
    colnames(osrmTable_iter_bind) <- c("h_bgs", "w_bgs", "dist (km)")
    
    bgs_table <- rbind(bgs_table, osrmTable_iter_bind)
    
  }

}
