st_over <- function (x, y) 
{
  require(sf)
  sapply(sf::st_intersects(x, y), function(z) if (length(z) == 
                                                  0) {
    NA_integer_
  }
  else {
    z[1]
  })
}

########################################################################################

point_to_csquare <- function (lon, lat, degrees) 
{
  if (length(lon) != length(lat)) 
    stop("length of longitude not equal to length of latitude")
  if (!degrees %in% c(10, 5, 1, 0.5, 0.1, 0.05, 0.01)) 
    stop("degrees specified not in range: c(10,5,1,0.5,0.1,0.05,0.01)")
  dims <- length(lon)
  quadrants <- array(NA, dim = c(4, 6, dims), dimnames = list(c("globalQuadrant", 
                                                                "intmQuadrant1", "intmQuadrant2", "intmQuadrant3"), c("quadrantDigit", 
                                                                                                                      "latDigit", "lonDigit", "latRemain", "lonRemain", "code"), 
                                                              seq(1, dims, 1)))
  quadrants["globalQuadrant", "quadrantDigit", ] <- 4 - (((2 * 
                                                             floor(1 + (lon/200))) - 1) * ((2 * floor(1 + (lat/200))) + 
                                                                                             1))
  quadrants["globalQuadrant", "latDigit", ] <- floor(abs(lat)/10)
  quadrants["globalQuadrant", "lonDigit", ] <- floor(abs(lon)/10)
  quadrants["globalQuadrant", "latRemain", ] <- round(abs(lat) - 
                                                        (quadrants["globalQuadrant", "latDigit", ] * 10), 7)
  quadrants["globalQuadrant", "lonRemain", ] <- round(abs(lon) - 
                                                        (quadrants["globalQuadrant", "lonDigit", ] * 10), 7)
  quadrants["globalQuadrant", "code", ] <- quadrants["globalQuadrant", 
                                                     "quadrantDigit", ] * 1000 + quadrants["globalQuadrant", 
                                                                                           "latDigit", ] * 100 + quadrants["globalQuadrant", "lonDigit", 
                                                                                           ]
  quadrants["intmQuadrant1", "quadrantDigit", ] <- (2 * floor(quadrants["globalQuadrant", 
                                                                        "latRemain", ] * 0.2)) + floor(quadrants["globalQuadrant", 
                                                                                                                 "lonRemain", ] * 0.2) + 1
  quadrants["intmQuadrant1", "latDigit", ] <- floor(quadrants["globalQuadrant", 
                                                              "latRemain", ])
  quadrants["intmQuadrant1", "lonDigit", ] <- floor(quadrants["globalQuadrant", 
                                                              "lonRemain", ])
  quadrants["intmQuadrant1", "latRemain", ] <- round((quadrants["globalQuadrant", 
                                                                "latRemain", ] - quadrants["intmQuadrant1", "latDigit", 
                                                                ]) * 10, 7)
  quadrants["intmQuadrant1", "lonRemain", ] <- round((quadrants["globalQuadrant", 
                                                                "lonRemain", ] - quadrants["intmQuadrant1", "lonDigit", 
                                                                ]) * 10, 7)
  quadrants["intmQuadrant1", "code", ] <- quadrants["intmQuadrant1", 
                                                    "quadrantDigit", ] * 100 + quadrants["intmQuadrant1", 
                                                                                         "latDigit", ] * 10 + quadrants["intmQuadrant1", "lonDigit", 
                                                                                         ]
  quadrants["intmQuadrant2", "quadrantDigit", ] <- (2 * floor(quadrants["intmQuadrant1", 
                                                                        "latRemain", ] * 0.2)) + floor(quadrants["intmQuadrant1", 
                                                                                                                 "lonRemain", ] * 0.2) + 1
  quadrants["intmQuadrant2", "latDigit", ] <- floor(quadrants["intmQuadrant1", 
                                                              "latRemain", ])
  quadrants["intmQuadrant2", "lonDigit", ] <- floor(quadrants["intmQuadrant1", 
                                                              "lonRemain", ])
  quadrants["intmQuadrant2", "latRemain", ] <- round((quadrants["intmQuadrant1", 
                                                                "latRemain", ] - quadrants["intmQuadrant2", "latDigit", 
                                                                ]) * 10, 7)
  quadrants["intmQuadrant2", "lonRemain", ] <- round((quadrants["intmQuadrant1", 
                                                                "lonRemain", ] - quadrants["intmQuadrant2", "lonDigit", 
                                                                ]) * 10, 7)
  quadrants["intmQuadrant2", "code", ] <- quadrants["intmQuadrant2", 
                                                    "quadrantDigit", ] * 100 + quadrants["intmQuadrant2", 
                                                                                         "latDigit", ] * 10 + quadrants["intmQuadrant2", "lonDigit", 
                                                                                         ]
  quadrants["intmQuadrant3", "quadrantDigit", ] <- (2 * floor(quadrants["intmQuadrant2", 
                                                                        "latRemain", ] * 0.2)) + floor(quadrants["intmQuadrant2", 
                                                                                                                 "lonRemain", ] * 0.2) + 1
  quadrants["intmQuadrant3", "latDigit", ] <- floor(quadrants["intmQuadrant2", 
                                                              "latRemain", ])
  quadrants["intmQuadrant3", "lonDigit", ] <- floor(quadrants["intmQuadrant2", 
                                                              "lonRemain", ])
  quadrants["intmQuadrant3", "latRemain", ] <- round((quadrants["intmQuadrant2", 
                                                                "latRemain", ] - quadrants["intmQuadrant3", "latDigit", 
                                                                ]) * 10, 7)
  quadrants["intmQuadrant3", "lonRemain", ] <- round((quadrants["intmQuadrant2", 
                                                                "lonRemain", ] - quadrants["intmQuadrant3", "lonDigit", 
                                                                ]) * 10, 7)
  quadrants["intmQuadrant3", "code", ] <- quadrants["intmQuadrant3", 
                                                    "quadrantDigit", ] * 100 + quadrants["intmQuadrant3", 
                                                                                         "latDigit", ] * 10 + quadrants["intmQuadrant3", "lonDigit", 
                                                                                         ]
  if (degrees == 10) 
    CSquareCodes <- quadrants["globalQuadrant", "code", ]
  if (degrees == 5) 
    CSquareCodes <- paste(quadrants["globalQuadrant", "code", 
    ], ":", quadrants["intmQuadrant1", "quadrantDigit", 
    ], sep = "")
  if (degrees == 1) 
    CSquareCodes <- paste(quadrants["globalQuadrant", "code", 
    ], ":", quadrants["intmQuadrant1", "code", ], sep = "")
  if (degrees == 0.5) 
    CSquareCodes <- paste(quadrants["globalQuadrant", "code", 
    ], ":", quadrants["intmQuadrant1", "code", ], ":", 
    quadrants["intmQuadrant2", "quadrantDigit", ], sep = "")
  if (degrees == 0.1) 
    CSquareCodes <- paste(quadrants["globalQuadrant", "code", 
    ], ":", quadrants["intmQuadrant1", "code", ], ":", 
    quadrants["intmQuadrant2", "code", ], sep = "")
  if (degrees == 0.05) 
    CSquareCodes <- paste(quadrants["globalQuadrant", "code", 
    ], ":", quadrants["intmQuadrant1", "code", ], ":", 
    quadrants["intmQuadrant2", "code", ], ":", quadrants["intmQuadrant3", 
                                                         "quadrantDigit", ], sep = "")
  if (degrees == 0.01) 
    CSquareCodes <- paste(quadrants["globalQuadrant", "code", 
    ], ":", quadrants["intmQuadrant1", "code", ], ":", 
    quadrants["intmQuadrant2", "code", ], ":", quadrants["intmQuadrant3", 
                                                         "code", ], sep = "")
  return(CSquareCodes)
}

##########################################################################################

get_adjacent_csquares <- function(input_csquares, csq_degrees = 0.05, diagonals = T) {
  
    input_points <- vmstools::CSquare2LonLat(input_csquares, degrees= csq_degrees)
    names(input_points) <- c("lat", "lon")
  
    if (diagonals == T ){
    
    adjacent_points <- data.frame(lon = c(input_points$lon + csq_degrees, input_points$lon + csq_degrees,input_points$lon + csq_degrees,
                                          input_points$lon, input_points$lon - csq_degrees, input_points$lon - csq_degrees, 
                                          input_points$lon - csq_degrees, input_points$lon),
                                  lat = c(input_points$lat - csq_degrees, input_points$lat, input_points$lat + csq_degrees,
                                          input_points$lat + csq_degrees, input_points$lat + csq_degrees, input_points$lat,
                                          input_points$lat - csq_degrees, input_points$lat - csq_degrees))
  } else if (diagonals ==F){
    
    adjacent_points <- data.frame(lon = c(input_points$lon + csq_degrees,
                                          input_points$lon, input_points$lon - csq_degrees, 
                                          input_points$lon),
                                  lat = c(input_points$lat, 
                                          input_points$lat + csq_degrees, input_points$lat,
                                          input_points$lat - csq_degrees))
  }
  
  output <- point_to_csquare(lon = adjacent_points$lon,
               lat = adjacent_points$lat, 
               degrees = csq_degrees)
  
  output <- output[output %in% input_csquares == FALSE]
  
  return(output)
  
}

######################################################################################

csquare_buffer <- function(vme.tab){
  
  if("sf" %in% is(vme.tab) == FALSE){print("To run, this function needs to be provided with a table of c-squares in an 'sf' object") 
    next
  }
  
  bboxes <- lapply(st_geometry(vme.tab), st_bbox)
  
  # Create a new rectangle buffered by 0.025 degrees for each original rectangle
  buffered_rects <- lapply(bboxes, function(bbox) {
    st_bbox(c(xmin = bbox[["xmin"]] - 0.02501, 
              ymin = bbox[["ymin"]] - 0.02501, 
              xmax = bbox[["xmax"]] + 0.02501, 
              ymax = bbox[["ymax"]] + 0.02501), 
            crs = st_crs(vme.tab))
  })
  
  # Convert the bboxes to rectangular polygons
  buffered_rects <- lapply(buffered_rects, st_as_sfc)
  buffered_rects <- do.call(c, buffered_rects)
  buffered_rects <- st_sf(geometry = buffered_rects)
  
  out.tab <- vme.tab
  
  st_geometry(out.tab) <- st_geometry(buffered_rects)
  
  
  return(out.tab)
  
}
