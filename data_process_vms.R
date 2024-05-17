## this script downloads VMS data from the ICES database, calculates mean SAR per c-square per year
## downloads the static gear footprint for each year, combines these into a single data frame, with
## one column for each year of sar values and one for each year, denoting presence/absence of static gears

## the static gear footprint function works one ecoregion at a time, so we need to stitch together the 
## output from multiple calls to the api
codelist <- getCodeList("Ecoregion")
all.areas <- c(codelist$Description)

## set up an object to store the annual tables into
out.tab <- NULL

for(i in 2009:2022){
  ## get the SAR data from the VMS database
  temp <- get_sar(year = i)

  ## remove unused columns, caluculate total sar for each c-square
  temp2 <- temp %>% 
    dplyr::select(year, surface_sar, c_square, wkt) %>%
    group_by(year, c_square, wkt) %>%
    summarise(SAR = sum(surface_sar))

  ## for each ecoregion
  temp4 <- map_dfr(all.areas, function(area) {
    ## get the passive gear footprint for year i from the ICES database
    temp3 <- get_passive_footprint(year = i, ecoregion = area)
    ## if there are static gears used in that ecoregion, extract the c-squares
    if (length(temp3) > 0 && !is.null(nrow(temp3))) {
       tibble(
       c_square = temp3$c_square,
       wkt = temp3$wkt
       )
    } else {
    tibble()
    }
  })
  ## add year and a numeric value to the static gear table
  temp4 <-  temp4 %>%
    mutate(year = i,
           static = 1)
  ## run a full join on the static and SAR tables, by c-square
  result <- full_join(temp2, temp4, by = c("c_square", "year")) %>%
    mutate(wkt = coalesce(wkt.x, wkt.y)) %>%
    select(year, c_square, SAR, static, wkt)
## exports the table for each year, so you end up with a "long" table
out.tab <- rbind(out.tab, result)

}

## reshapes the table to be "wide", with one column of SAR and one of static gear presence for each year
result <- out.tab %>%
  pivot_wider(
    names_from = year,
    values_from = c(SAR, static),
    names_glue = "{.value}_{year}",
    values_fill = NA
  ) %>%
  dplyr::select(c_square, everything(), wkt)

## exports the result in the format expected in the rest of the original code.
write.table(result, "All_VMS_datacall_2023(2009-2022).csv", sep = ";", )
