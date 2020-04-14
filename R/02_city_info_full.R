##### 02. CITY INFORMATION INPUT ###############################################

source("R/01_helper_functions.R")

# note that there are two Glendales
cityname  <- 
  c("Albuquerque", "Anaheim", "Anchorage", "Arlington", "Atlanta", "Aurora", 
    "Austin", "Bakersfield", "Baltimore", "Baton Rouge", "Birmingham", "Boise", 
    "Boston", "Buffalo", "Cincinnati", "Chandler", "Charlotte", "Chesapeake",
    "Chicago", "Chula Vista", "Cleveland", "Colorado Springs", "Columbus", 
    "Corpus Christi",  "Dallas", "Denver", "Des Moines", "Detroit", "Durham", 
    "El Paso", "Fayetteville", "Fontana", "Fort Wayne", "Fort Worth", "Fremont", 
    "Fresno", "Garland", "Gilbert", "Glendale", "Glendale", "Grand Rapids", 
    "Greensboro", "Henderson", "Hialeah", "Honolulu", "Houston", 
    "Huntington Beach", "Indianapolis", "Irvine", "Irving", "Jacksonville", 
    "Jersey City", "Kansas City", "Laredo", "Las Vegas", "Lexington", 
    "Lincoln", "Long Beach", "Los Angeles", "Louisville", "Lubbock", 
    "Madison", "Memphis", "Mesa", "Miami", "Milwaukee", "Minneapolis", 
    "Modesto", "Moreno Valley", "Nashville", "New Orleans", "New York", 
    "Newark", "Norfolk", "North Las Vegas", "Oakland", "Oklahoma City", "Omaha",
    "Orlando", "Oxnard", "Philadelphia", "Phoenix", "Pittsburgh", "Plano", 
    "Portland", "Raleigh", "Reno", "Richmond", "Riverside", "Rochester", 
    "Sacramento", "Saint Louis", "Saint Paul", "Saint Petersburg", 
    "Salt Lake City", "San Antonio", "San Bernardino", "San Diego", 
    "San Francisco", "San Jose", "Santa Ana", "Santa Clarita", "Scottsdale", 
    "Seattle", "Spokane", "Stockton", "Tacoma", "Tampa", "Toledo", "Tucson", 
    "Tulsa", "Virginia Beach", "Washington", "Wichita", "Winston Salem")

# Neighbourhood geometries
neighbourhoods <- list("Albuquerque" = import_puma("NM") %>% 
                            filter(str_detect(neighbourhood, "Albuquerque City")),
                          "Anaheim" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Anaheim City")), 
                          "Anchorage" = import_puma("AK") %>% 
                            filter(str_detect(neighbourhood, "Anchorage")),
                          "Arlington" = import_puma("TX") %>% 
                            filter(str_detect(neighbourhood, "Arlington")),
                          "Atlanta" = import_puma("GA") %>% 
                            filter(str_detect(neighbourhood, "Atlanta City")), 
                          "Aurora" = import_puma("CO") %>% 
                            filter(str_detect(neighbourhood, "Aurora")), 
                          "Austin" = import_puma("TX") %>% 
                            filter(str_detect(neighbourhood, "Austin")), 
                          "Bakersfield" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Bakersfield")), 
                          "Baltimore" = import_puma("MD") %>% 
                            filter(str_detect(neighbourhood, "Baltimore City")), 
                          "Baton Rouge" = import_puma("LA") %>% 
                            filter(str_detect(neighbourhood, "Baton Rouge City")), 
                          "Birmingham" = import_puma("AL") %>% 
                            filter(str_detect(neighbourhood, "Birmingham")), 
                          "Boise" = import_puma("ID") %>% 
                            filter(str_detect(neighbourhood, "Boise")), 
                          "Boston" = import_puma("MA") %>% 
                            filter(str_detect(neighbourhood, "Boston")), 
                          "Buffalo" = import_puma("NY") %>% 
                            filter(str_detect(neighbourhood, "Buffalo")), 
                          "Cincinnati" = import_puma("OH") %>% 
                            filter(str_detect(neighbourhood, "Cinc")), 
                          "Chandler" = import_puma("AZ") %>% 
                            filter(str_detect(neighbourhood, "Chandler")), 
                          "Charlotte" = import_puma("NC") %>% 
                            filter(str_detect(neighbourhood, "Charlotte")), 
                          "Chesapeake" = import_puma("VA") %>% 
                            filter(str_detect(neighbourhood, "Chesa")),
                          "Chicago" = import_puma("IL") %>% 
                            filter(str_detect(neighbourhood, "Chicago")), 
                          "Chula Vista" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Chula")),
                          "Cleveland" = import_puma("OH") %>% 
                            filter(str_detect(neighbourhood, "Cleveland")), 
                          "Colorado Springs" = import_puma("CO") %>% 
                            filter(str_detect(neighbourhood, "Colorado Springs")),
                          "Columbus" = import_puma("OH") %>% 
                            filter(str_detect(neighbourhood, "Columbus")), 
                          "Corpus Christi" = import_puma("TX") %>% 
                            filter(str_detect(neighbourhood, "Corpus")),  
                          "Dallas" = import_puma("TX") %>% 
                            filter(str_detect(neighbourhood, "Dallas")) %>% 
                            filter(str_detect(neighbourhood, "Dallas County", 
                                              negate = TRUE)),
                          "Denver" = import_puma("CO") %>% 
                            filter(str_detect(neighbourhood, "Denver")), 
                          "Des Moines" = import_puma("IA") %>% 
                            filter(str_detect(neighbourhood, "Des Moines City") |
                                     str_detect(neighbourhood, "West Des Moines")), 
                          "Detroit" = import_puma("MI") %>% 
                            filter(str_detect(neighbourhood, "Detroit")), 
                          "Durham" = import_puma("NC") %>% 
                            filter(str_detect(neighbourhood, "Durham")), 
                          "El Paso" = import_puma("TX") %>% 
                            filter(str_detect(neighbourhood, "Paso")) %>% 
                            filter(str_detect(neighbourhood, "County", 
                                            negate = TRUE)), 
                          "Fayetteville" = import_puma("NC") %>% 
                            filter(str_detect(neighbourhood, "Fayette")), 
                          "Fontana" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Fontana")), 
                          "Fort Wayne" = import_puma("IN") %>% 
                            filter(str_detect(neighbourhood, "Fort Wayne")) %>% 
                            filter(str_detect(neighbourhood, "Outside Fort Wayne City", 
                                            negate = TRUE)), 
                          "Fort Worth" = import_puma("TX") %>% 
                            filter(str_detect(neighbourhood, "Fort Worth")), 
                          "Fremont" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Fremont")), 
                          "Fresno" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Fresno City")), 
                          "Garland" = import_puma("TX") %>% 
                            filter(str_detect(neighbourhood, "Garland")), 
                          "Gilbert" = import_puma("AZ") %>% 
                            filter(str_detect(neighbourhood, "Gilbert")), 
                          "Glendale AZ" = import_puma("AZ") %>% 
                            filter(str_detect(neighbourhood, "Glendale")), 
                          "Glendale CA" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Glendale")), 
                          "Grand Rapids" = import_puma("MI") %>% 
                            filter(str_detect(neighbourhood, "Grand Rapids")), 
                          "Greensboro" = import_puma("NC") %>% 
                            filter(str_detect(neighbourhood, "Greensboro")), 
                          "Henderson" = import_puma("NV") %>% 
                            filter(str_detect(neighbourhood, "Henderson")), 
                          "Hialeah" = import_puma("FL") %>% 
                            filter(str_detect(neighbourhood, "Hialeah")), 
                          "Honolulu" = import_puma("HI") %>% 
                            filter(str_detect(neighbourhood, "Honolulu")) %>% 
                            filter(str_detect(neighbourhood, "Rural", 
                                             negate = TRUE)),
                          "Houston" = import_puma("TX") %>% 
                            filter(str_detect(neighbourhood, "Houston")) %>% 
                            filter(str_detect(neighbourhood, "Houston-", 
                                              negate = TRUE)), 
                          "Huntington Beach" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Huntington Beach")), 
                          "Indianapolis" = import_puma("IN") %>% 
                            filter(str_detect(neighbourhood, "Indianapolis")), 
                          "Irvine" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Irvine")), 
                          "Irving" = import_puma("TX") %>% 
                            filter(str_detect(neighbourhood, "Irving")), 
                          "Jacksonville" = import_puma("FL") %>% 
                            filter(str_detect(neighbourhood, "Jacksonville")), 
                          "Jersey City" = import_puma("NJ") %>% 
                            filter(str_detect(neighbourhood, "Jersey City")), 
                          "Kansas City" = import_puma("MO") %>% 
                            filter(str_detect(neighbourhood, "Kansas")), 
                          "Laredo" = import_puma("TX") %>% 
                            filter(str_detect(neighbourhood, "Laredo")), 
                          "Las Vegas" = import_puma("NV") %>% 
                            filter(str_detect(neighbourhood, "Vegas")) %>% 
                            filter(str_detect(neighbourhood, "North Las Vegas", 
                                              negate = TRUE)), 
                          "Lexington" = import_puma("KY") %>% 
                            filter(str_detect(neighbourhood, "Lexington")), 
                          "Lincoln" = import_puma("NE") %>% 
                            filter(str_detect(neighbourhood, "Lincoln")), 
                          "Long Beach" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Long Beach")), 
                          "Los Angeles" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "LA City")), 
                          "Louisville" = import_puma("KY") %>% 
                            filter(str_detect(neighbourhood, "Louisville") &
                                     (str_detect(neighbourhood, "Northwest")|
                                     str_detect(neighbourhood, "Central"))), 
                          "Lubbock" = import_puma("TX") %>% 
                            filter(str_detect(neighbourhood, "Lubbock City")), 
                          "Madison" = import_puma("WI") %>% 
                            filter(str_detect(neighbourhood, "Madison")), 
                          "Memphis" = import_puma("TN") %>% 
                            filter(str_detect(neighbourhood, "Memphis")), 
                          "Mesa" = import_puma("AZ") %>% 
                            filter(str_detect(neighbourhood, "Mesa")), 
                          "Miami" = import_puma("FL") %>% 
                            filter(str_detect(neighbourhood, "Miami City") |
                                   str_detect(neighbourhood, 
                                              "Miami Beach City") |
                                   str_detect(neighbourhood, 
                                              "Miami Springs City") |
                                   str_detect(neighbourhood, 
                                              "Miami Gardens City") |
                                   str_detect(neighbourhood, "Greater Miami")),
                          "Milwaukee" = import_puma("WI") %>% 
                            filter(str_detect(neighbourhood, "Milwaukee City")),
                          "Minneapolis" = import_puma("MN") %>% 
                            filter(str_detect(neighbourhood, "Minneapolis")), 
                          "Modesto" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Modesto")), 
                          "Moreno Valley" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Moreno")), 
                          "Nashville" = import_puma("TN") %>% 
                            filter(str_detect(neighbourhood, "Nashville")), 
                          "New Orleans" =  import_puma("LA") %>% 
                            filter(str_detect(neighbourhood, "New Orleans")), 
                          "New York" = import_puma("NY") %>% 
                            filter(str_detect(neighbourhood, "NYC")), 
                          "Newark" = import_puma("NJ") %>% 
                            filter(str_detect(neighbourhood, "Newark")),
                          "Norfolk" = import_puma("VA") %>% 
                            filter(str_detect(neighbourhood, "Norfolk")), 
                          "North Las Vegas" = import_puma("NV") %>% 
                            filter(str_detect(neighbourhood, "North Las Vegas")), 
                          "Oakland" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Oakland")), 
                          "Oklahoma City" = import_puma("OK") %>% 
                            filter(str_detect(neighbourhood, "Oklahoma City")), 
                          "Omaha" = import_puma("NE") %>% 
                            filter(str_detect(neighbourhood, "Omaha")),
                          "Orlando" = import_puma("FL") %>% 
                            filter(str_detect(neighbourhood, "Orlando")), 
                          "Oxnard" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Oxnard")), 
                          "Philadelphia" = import_puma("PA") %>% 
                            filter(str_detect(neighbourhood, "Phila")), 
                          "Phoenix" = import_puma("AZ") %>% 
                            filter(str_detect(neighbourhood, "Phoenix")), 
                          "Pittsburgh" = import_puma("PA") %>% 
                            filter(str_detect(neighbourhood, "Pittsburgh")), 
                          "Plano" = import_puma("TX") %>% 
                            filter(str_detect(neighbourhood, "Plano")), 
                          "Portland" = import_puma("OR") %>% 
                            filter(str_detect(neighbourhood, "Portland")), 
                          "Raleigh" = import_puma("NC") %>% 
                            filter(str_detect(neighbourhood, "Raleigh")), 
                          "Reno" = import_puma("NV") %>% 
                            filter(str_detect(neighbourhood, "Reno")), 
                          "Richmond" = import_puma("VA") %>% 
                            filter(str_detect(neighbourhood, "Richmond")), 
                          "Riverside" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Riverside City")), 
                          "Rochester" = import_puma("NY") %>% 
                            filter(str_detect(neighbourhood, "Rochester")), 
                          "Sacramento" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Sacramento Cit")), 
                          "Saint Louis" = import_puma("MO") %>% 
                            filter(str_detect(neighbourhood, "Louis City")), 
                          "Saint Paul" = import_puma("MN") %>% 
                            filter(str_detect(neighbourhood, "Paul")), 
                          "Saint Petersburg" = import_puma("FL") %>% 
                            filter(str_detect(neighbourhood, "Petersburg")), 
                          "Salt Lake City" = import_puma("UT") %>% 
                            filter(str_detect(neighbourhood, "Salt Lake Cit")), 
                          "San Antonio" = import_puma("TX") %>% 
                            filter(str_detect(neighbourhood, "Antonio")) , 
                          "San Bernardino" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Bernardino City")), 
                          "San Diego" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "San Diego City") |
                                     str_detect(neighbourhood, "Dieguito") |
                                     str_detect(neighbourhood, 
                                                "Rancho Bernardo") |
                                     str_detect(neighbourhood, "Navajo")), 
                          "San Francisco" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "San Francisco")), 
                          "San Jose" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "San Jose")), 
                          "Santa Ana" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Santa Ana")), 
                          "Santa Clarita" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Santa Clarita")), 
                          "Scottsdale" = import_puma("AZ") %>% 
                            filter(str_detect(neighbourhood, "Scottsdale")), 
                          "Seattle" = import_puma("WA") %>% 
                            filter(str_detect(neighbourhood, "Seattle")), 
                          "Spokane" = import_puma("WA") %>% 
                            filter(str_detect(neighbourhood, "Spokane City")), 
                          "Stockton" = import_puma("CA") %>% 
                            filter(str_detect(neighbourhood, "Stockton")), 
                          "Tacoma" = import_puma("WA") %>% 
                            filter(str_detect(neighbourhood, "Tacoma")), 
                          "Tampa" = import_puma("FL") %>% 
                            filter(str_detect(neighbourhood, "Tampa")), 
                          "Toledo" = import_puma("OH") %>% 
                            filter(str_detect(neighbourhood, "Toledo")), 
                          "Tucson" = import_puma("AZ") %>% 
                            filter(str_detect(neighbourhood, "Tucson")), 
                          "Tulsa" = import_puma("OK") %>% 
                            filter(str_detect(neighbourhood, "Tulsa")), 
                          "Virginia Beach" = import_puma("VA") %>% 
                            filter(str_detect(neighbourhood, "Virginia Beach")), 
                          "Washington" = import_puma("DC"),  
                          "Wichita" =  import_puma("KS") %>% 
                            filter(str_detect(neighbourhood, "Wichita")), 
                          "Winston Salem" = import_puma("NC") %>% 
                            filter(str_detect(neighbourhood, "Winston-Salem City")))
                          

                          