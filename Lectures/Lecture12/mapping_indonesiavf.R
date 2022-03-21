################################################################################
##
## [ PROJ ] Lecture 11: Basic Maps using tmap 
## [ FILE ] mapping_indonesian_village_fund.r
## [ AUTH ] < YOUR NAME >
## [ INIT ] < Nov 24, 2021 >
##
################################################################################

#Mapping Indonesian village fund allocations (2018)


## -----------------------------------------------------------------------------
## libraries
## -----------------------------------------------------------------------------

#install.packages("tmap")

library(tmap)
library(tidyverse)


## -----------------------------------------------------------------------------
## directory paths
## -----------------------------------------------------------------------------

getwd()


## -----------------------------------------------------------------------------
## get shapefile (geometry for Indonesia)
## -----------------------------------------------------------------------------



indonesia <- readRDS('indonesia.rds')
test <- st_read("indonesia.shp")

test2 <- st_read("philly_tracts_384_crs-4326.shp")


  #shape file downloaded from https://gadm.org/download_country_v3.html

#create a basic tmap object t that shows Indonesia's regencies shaded by FID
t <- tm_shape(indonesia) + tm_borders()
t


## -----------------------------------------------------------------------------
## Load VF data, prepare to join to shapefile, create joined data frame
## -----------------------------------------------------------------------------

load("vf_joined.RData")

#before joining, let's perform some data cleaning

#first step: to make indonesia joinable with vf_joined, 
#let's add Kabupaten to regency name which does not start with Kota
#or Kota Baru (since Kota Baru is a Kabupaten, an exception!)
  indonesia <- indonesia %>%
    mutate(name = ifelse(word(name, 1) != 'Kota' | name == 'Kota Baru',
                         str_c('Kabupaten ', name), name))

#also, Kabupaten Banjar where FID = 83 should be Kota Banjar
  indonesia[indonesia$FID == 83, "name"] <- 'Kota Banjar'

#second step: let's join and see which regencies in vf_joined have no match
  vf_joined2 <- indonesia %>%
    right_join(vf_joined, by = c('name' = 'regency'))

#which regencies have no match?
  no_match <- vf_joined2[is.na(vf_joined2$FID),]
  
#do a manual check and perform a mapping correspondingly (same as Week 8)
  to <- c('Kota Banda Aceh', 'Kota Langsa', 'Kota Lhokseumawe', 'Kota Sabang',
          'Kota Subulussalam', 'Kota Denpasar', 'Kabupaten Muko Muko', 'Kabupaten Pahuwato',
          'Kabupaten Batanghari', 'Kabupaten Tanjung Jabung Barat', 'Kabupaten Tanjung Jabung Timur',
          'Kota Sungai Penuh', 'Kota Batu', 'Kabupaten Mempawah',
          'Kabupaten Kotabaru', 'Kabupaten Tulang Bawang', 'Kota Ambon', 
          'Kota Tual', 'Kota Tidore Kepulauan', 'Kabupaten Fak Fak', 
          'Kabupaten Pangkajene Kepulauan', 'Kabupaten Tojo Una Una', 'Kabupaten Toli Toli', 
          'Kabupaten Kep. Siau Tagulandang Biaro', 'Kota Kotamobagu', 'Kota Pariaman', 
          'Kota Sawahlunto', 'Kabupaten Banyuasin', 'Kota Prabumulih',
          'Kabupaten Pakpak Bharat', 'Kota Gunungsitoli', 'Kota Padangsidimpuan')
  
  from <- c('Kabupaten Banda Aceh', 'Kabupaten Langsa', 'Kabupaten Lhokseumawe', 'Kabupaten Sabang',
            'Kabupaten Subulussalam', 'Kabupaten Denpasar', 'Kabupaten Mukomuko', 'Kabupaten Pohuwato',
            'Kabupaten Batang Hari', 'Kabupaten Tanjung Jabung B', 'Kabupaten Tanjung Jabung T',
            'Kabupaten Sungai Penuh', 'Kabupaten Batu', 'Kabupaten Pontianak',
            'Kabupaten Kota Baru', 'Kabupaten Tulangbawang', 'Kabupaten Ambon', 
            'Kabupaten Tual', 'Kabupaten Tidore Kepulauan', 'Kabupaten Fakfak', 
            'Kabupaten Pangkajene Dan Kepulauan', 'Kabupaten Tojo Una-Una', 'Kabupaten Toli-Toli', 
            'Kabupaten Siau Tagulandang Biaro', 'Kabupaten Kotamobagu', 'Kabupaten Pariaman', 
            'Kabupaten Sawahlunto', 'Kabupaten Banyu Asin', 'Kabupaten Prabumulih',
            'Kabupaten Pakpak Barat', 'Kabupaten Gunungsitoli', 'Kabupaten Padangsidimpuan')

  indonesia$name <- plyr::mapvalues(indonesia$name, from = from, to = to)

  
#let's do the join once again
  vf_joined2 <- indonesia %>%
    right_join(vf_joined, by = c('name' = 'regency'))

#which regencies have no match?
  no_match <- vf_joined2[is.na(vf_joined2$FID),]
  
#we managed to reduce 49 no-matches to 17 no-matches 
#(mostly new regencies not contained in the shapefile)

#now, we are ready to map!


## -----------------------------------------------------------------------------
## Map the VF data!
## -----------------------------------------------------------------------------

vf_joined_final <- vf_joined2 %>%
  filter(!is.na(FID))

?tmap
  
  t
  
#explore what each component does!
  t2 <- tm_shape(vf_joined_final) + tm_fill('vfund_per_poor') + tm_borders() +
    tm_legend(outside = TRUE) 
  t2
  

  g2 <- ggplot(vf_joined_final) + 
    geom_sf(aes(fill = vfund_per_poor)) 
  

  #modifying the color schemes
#suppose we want to have one color for every multiple of 10,000,000

#explore what each component does!
  t3 <- tm_shape(vf_joined_final) + 
    tm_fill('vfund_per_poor', palette = 'Reds',
            style = 'fixed', breaks = c(0, 10000000, 20000000, 30000000, 40000000),
            labels = c('Under 10M', '10M to 20M', '20M to 30M', 'Above 30M'),
            title = 'Fund per poor individual (Rp)') + 
    tm_borders() +
    tm_legend(outside = TRUE) 
  t3

#suppose we want to split vfund/poor into quartiles (up to 25th pctile, 26-50, etc.)
  qt <- quantile(vf_joined_final$vfund_per_poor, probs = c(0, 0.25, 0.5, 0.75, 1))

  t4 <- tm_shape(vf_joined_final) + 
    tm_fill('vfund_per_poor', palette = 'Reds',
            style = 'fixed', breaks = qt,
            title = 'Fund per poor individual (Rp)') + 
    tm_borders() +
    tm_legend(outside = TRUE)
  t4
