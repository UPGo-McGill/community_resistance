######################################### CITY INFORMATION INPUT ########################################################

source("R/01_helper_functions.R")

# List of city names
cityname_canada <- c("Calgary", "Edmonton", "Gatineau", "Halifax", 
                     "Hamilton", "Kitchener", "London", "Montreal", 
                     "Oshawa", "Ottawa", "Quebec City", "Regina", 
                     "Saskatoon", "Sainte Catharines", "Toronto", 
                     "Vancouver", "Victoria", "Windsor", "Winnipeg")

cityname_us <- c("Albuquerque", "Anaheim", "Anchorage", "Arlington", "Atlanta", 
                 "Aurora", "Austin", "Bakersfield", "Baltimore",
                 "Baton Rouge", "Birmingham", "Boise", "Boston",
                 "Buffalo", "Cincinnati", "Chandler", "Charlotte", "Chesapeake",
                 "Chicago", "Chula Vista", "Cleveland", "Colorado Springs", 
                 "Columbus", "Corpus Christi",  "Dallas", "Denver", "Des Moines", 
                 "Detroit", "Durham", "El Paso", "Fayetteville", "Fontana", 
                 "Fort Wayne", "Fort Worth", "Fremont", "Fresno", "Garland", 
                 "Gilbert", "Glendale", "Grand Rapids", 
                 "Greensboro", "Henderson", "Hialeah", "Honolulu", "Houston", 
                 "Huntington Beach", "Indianapolis", "Irvine", "Irving", "Jacksonville", 
                 "Jersey City", "Kansas City", "Laredo", "Las Vegas", "Lexington", 
                 "Lincoln", "Long Beach", "Los Angeles", "Louisville", "Lubbock", 
                 "Madison", "Memphis", "Mesa", "Miami", "Milwaukee", "Minneapolis", "Modesto", 
                 "Moreno Valley", "Nashville", "New Orleans", "New York", "Newark",
                 "Norfolk", "North Las Vegas", "Oakland", "Oklahoma City", "Omaha",
                 "Orlando", "Oxnard", "Philidelphia", "Phoenix", "Pittsburg", "Plano", 
                 "Portland", "Raleigh", "Reno", "Richmond", "Riverside", "Rochester", 
                 "Sacramento", "Saint Louis", "Saint Paul", "Saint Petersburg", "Salt Lake City", 
                 "San Antonio", "San Bernardino", "San Diego", "San Francisco", "San Jose", 
                 "Santa Ana", "Santa Clarita", "Scottsdale", "Seattle", "Spokane", "Stockton", 
                 "Tacoma", "Tampa", "Toledo", "Tucson", "Tulsa", "Virginia Beach", "Washington", 
                 "Wichita", "Winston-Salem")

cityname = c(cityname_canada, cityname_us)
