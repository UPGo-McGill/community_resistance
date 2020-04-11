##################### LEXISNIEXIS SCRAPING #####################################
################################################################################

### Setup ######################################################################

library(tidyverse)
library(upgo)

# Set up parameters
citynames  <- 
  c(
    # "Albuquerque", "Anaheim", "Anchorage", "Arlington", "Atlanta", "Aurora", 
    # "Austin", "Bakersfield", "Baltimore", "Baton Rouge", "Birmingham", "Boise", 
    # "Boston", "Buffalo", "Cincinnati", "Chandler", "Charlotte", "Chesapeake",
    # "Chicago", "Chula Vista", "Cleveland", "Colorado Springs", "Columbus", 
    # "Corpus Christi",  "Dallas", "Denver", "Des Moines", "Detroit", "Durham", 
    # "El Paso", "Fayetteville", "Fontana", "Fort Wayne", "Fort Worth", "Fremont",
    # "Fresno", "Garland", "Gilbert", "Glendale", "Grand Rapids", "Greensboro", 
    # "Henderson", "Hialeah", "Honolulu", "Houston", "Huntington Beach", 
    # "Indianapolis", "Irvine", "Irving", "Jacksonville", "Jersey City", 
    # "Kansas City", "Laredo", "Las Vegas", "Lexington", "Lincoln", "Long Beach", 
    # "Los Angeles", "Louisville", "Lubbock", "Madison", "Memphis", 
    # "Mesa", "Miami", "Milwaukee", "Minneapolis", "Modesto", "Moreno Valley", 
    # "Nashville", "New Orleans", "New York", "Newark", "Norfolk", 
    # "North Las Vegas", "Oakland", "Oklahoma City", "Omaha", "Orlando", "Oxnard",
    # "Philadelphia", "Phoenix", "Pittsburgh", "Plano", "Portland", "Raleigh", 
    # "Reno", "Richmond", "Riverside", "Rochester", "Sacramento", "Saint Louis", 
    # "Saint Paul", "Saint Petersburg", "Salt Lake City", "San Antonio", 
    # "San Bernardino", "San Diego", "San Francisco", "San Jose", "Santa Ana", 
    # "Santa Clarita", "Scottsdale", "Seattle", "Spokane", "Stockton", "Tacoma", 
    # "Tampa", "Toledo", "Tucson", "Tulsa", "Virginia Beach", "Washington", 
    "Wichita", "Winston Salem")

timeout <- 5
upgo_scrape_disconnect()


### Initialize server and open browser #########################################

upgo_scrape_connect()

eCaps <- list(chromeOptions = list(args = c(
  # "--headless",
  "--disable-gpu", "--window-size=1280,800"), w3c = FALSE))

remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost", 
                                 browserName = "chrome", 
                                 port = 4444L, 
                                 extraCapabilities = eCaps)

remDr$open()
remDr$setImplicitWaitTimeout(5000)


### Main loop ##################################################################

for (cityname in citynames) {
  
  ### Perform search and narrow down results ###################################
  
  message(crayon::magenta(crayon::bold((glue::glue(
    "Beginning scrape of {cityname}. {Sys.time()}"
    )))))
  
  ## Do the search
  
  message(crayon::magenta((glue::glue(
    "Navigating to page and entering search term."
    ))))
  remDr$navigate(
    "https://proxy.library.mcgill.ca/login?url=http://nexisuni.com")
  Sys.sleep(timeout)
  
  webElem <- remDr$findElement(using = "id", value = "searchTerms")
  webElem$clickElement()
  webElem$sendKeysToElement(list("airbnb and ", cityname))
  webElem <- remDr$findElement(using = "id", value = "mainSearch")
  webElem$clickElement()
  Sys.sleep(timeout)
  
  
  ## Choose date range
  
  message(crayon::magenta((glue::glue("Setting date range."))))
  
  # Test if the date entry fields are visible, and open area if not
  tryCatch(
    suppressMessages({
      webElem <- remDr$findElement(using = "class name", value = "min-val")
      if (!unlist(webElem$isElementDisplayed())) {
        webElem <- remDr$findElement(
          using = "xpath", value = "//*[@data-filtertype = 'datestr-news']")
        webElem$clickElement()
        }}), error = function(e) {
          webElem <- remDr$findElement(
            using = "xpath", value = "//*[@data-filtertype = 'datestr-news']")
        webElem$clickElement()
      })
  
  # Enter start date
  webElem <- remDr$findElement(using = "class name", value = "min-val")
  webElem$sendKeysToElement(
    list("\u0008", "\u0008", "\u0008", "\u0008", "\u0008", 
         "\u0008", "\u0008", "\u0008", "\u0008", "\u0008"))
  webElem$sendKeysToElement(list("01/01/2015"))
  
  # Enter end date
  webElem <- remDr$findElement(using = "class name", value = "max-val")
  webElem$clickElement()
  webElem$sendKeysToElement(
    list("\u0008", "\u0008", "\u0008", "\u0008", "\u0008", 
         "\u0008", "\u0008", "\u0008", "\u0008", "\u0008"))
  webElem$sendKeysToElement(list("12/31/2019"))
  
  Sys.sleep(timeout/5)
  
  # Save parameters
  webElem <- remDr$findElement(using = "class name", value = "save")
  webElem$clickElement()
  Sys.sleep(timeout)
  
  ## Select English results
  
  message(crayon::magenta((glue::glue("Narrowing results to English."))))
  
  tryCatch(
    suppressMessages({
      webElem <- 
        remDr$findElement(
          using = "xpath", 
          value = "/html/body/main/div/main/div[2]/div/div[1]/aside/div[2]/div/ul//span[text() = 'English']")
      if (!unlist(webElem$isElementDisplayed())) {
        webElem <- 
          remDr$findElement(using = "id", value = "podfiltersbuttonlanguage")
        webElem$clickElement()
      }}), error = function(e) {
      webElem <- 
        remDr$findElement(using = "id", value = "podfiltersbuttonlanguage")
      webElem$clickElement()
      })

  webElem <- remDr$findElement(using = "xpath", value = 
    "/html/body/main/div/main/div[2]/div/div[1]/aside/div[2]/div/ul//span[text() = 'English']")
  webElem$clickElement()
  
  Sys.sleep(timeout)
  
  ### Loop through search results and select ###################################
  
  n <- 0
  keep_selecting_outer <- TRUE
  
  while (keep_selecting_outer) {
    
    keep_selecting_inner <- TRUE
    try_next <- TRUE
    i = 0
    message(crayon::magenta((glue::glue(
      "Selecting articles {n}01 through {n+1}00. "
      ))), appendLF = FALSE)
    
    while (keep_selecting_inner) {
      
      webElem <- remDr$findElement(using = "xpath", value = 
        "/html/body/main/div/main/div[2]/div/div[2]/div[2]/form/div[1]/div/ul[1]/li[1]/input")
      webElem$clickElement()
      
      if (try_next) {
        
        webElem <- remDr$findElement(using = "css selector", 
                                     value = "a[data-action='nextpage']")
        suppressMessages(try(webElem$clickElement(), silent = TRUE))
        suppressMessages(try(webElem$clickElement(), silent = TRUE))
        suppressMessages(try(webElem$clickElement(), silent = TRUE))
        Sys.sleep(timeout)
        
        try_next <- 
          suppressMessages(
            tryCatch({
              remDr$findElement(
                "css selector", "a[data-action='nextpage']"); TRUE
              }, error = function(e) FALSE))
        
        i = i + 1
        
        if (i == 10) keep_selecting_inner <- FALSE
      } else keep_selecting_inner <- FALSE
    }
    
    
    ### Download results #######################################################
    
    keep_selecting_outer <- 
      suppressMessages(
        tryCatch({
          remDr$findElement(
            "css selector", value = "a[data-action='nextpage']"); TRUE
          }, error = function(e) FALSE))
    
    message(crayon::magenta((glue::glue("Preparing download. "))), 
            appendLF = FALSE)
    
    webElem <- remDr$findElement(using = "xpath", value = 
      "/html/body/main/div/main/div[2]/div/div[2]/div[2]/form/div[1]/div/ul[1]/li[4]/ul/li[3]/button/span[1]")
    webElem$clickElement()
    Sys.sleep(timeout)
    
    webElem <- remDr$findElement(using = "id", value = "Docx")
    webElem$clickElement()
    
    webElem <- remDr$findElement(using = "id", "FileName")
    webElem$clearElement()
    webElem$sendKeysToElement(list(glue::glue("{cityname}_{n}")))
    n <- n+1
    
    message(crayon::magenta((glue::glue("Downloading articles."))))
    
    webElem <- remDr$findElement(using = "xpath", 
                                 value = "//*[@data-action = 'download']")
    webElem$clickElement()
    Sys.sleep(timeout)

    # Switch back to primary window.
    remDr$switchToWindow(remDr$getWindowHandles()[[1]])
  }
  
  
  ### Clean up #################################################################
  
  message(crayon::magenta((glue::glue("Downloads complete; cleaning up. \n \n"
    ))))
  windows_to_close <- max(length(remDr$getWindowHandles()) - 3, 0)
  
  for (j in seq_len(windows_to_close)) {
    remDr$switchToWindow(remDr$getWindowHandles()[[2]])
    remDr$closeWindow()
    Sys.sleep(timeout / 8)
  }

  remDr$switchToWindow(remDr$getWindowHandles()[[1]])
  
  
}

  upgo_scrape_disconnect()







### SCRAPING ####


library(tidyverse)
library(upgo)


## Parameters

i <- 0
cityname <- "Montreal"
timeout <- 3

## Get to the search page

upgo_scrape_connect()

eCaps <- list(chromeOptions = list(args = c("--disable-gpu", "--window-size=1280,800"), w3c = FALSE))
remDr <- RSelenium::remoteDriver(browserName = "chrome", extraCapabilities = eCaps)

remDr$open()
remDr$setImplicitWaitTimeout(5000)


loop_status <- TRUE


### MAIN LOOP ##################################################################

while (loop_status) {
  
  
  remDr$navigate(
    "https://proxy.library.mcgill.ca/login?url=http://global.factiva.com/")
  Sys.sleep(timeout)
  
  ## Do the search
  
  # Enter search text
  webElem <- remDr$findElement(using = "class name", value = "ace_text-input")
  webElem$clickElement()
  webElem$sendKeysToElement(list("airbnb and ", cityname))
  Sys.sleep(timeout)
  
  # Choose date range
  webElem <- remDr$findElement(using = "xpath", 
                               value = "//*/option[@value = 'Custom']")
  webElem$clickElement()
  Sys.sleep(timeout)
  
  # Enter date values
  date_values <- c("frd", "frm", "fry", "tod", "tom", "toy")
  date_entries <- c("01", "01", "2015", "31", "12", "2019")
  date_inputs <- map2(date_values, date_entries, ~{
    webElem <- remDr$findElement(using = "id", value = .x)
    webElem$sendKeysToElement(list(.y))
  })
  Sys.sleep(timeout)
  
  # Search for entries
  webElem <- remDr$findElement(using = "id", value = "btnSBSearch")
  webElem$clickElement()
  Sys.sleep(timeout)
  
  ## Filter articles to English
  
  # Get all the cItems
  values <- remDr$findElements(using = "class name", value = "cItem")
  
  # Find the correct one, by search for the text "English
  webElem <- 
    values[[which(map_lgl(values, ~{
      str_detect(.x$getElementAttribute("outerHTML")[[1]], "English")
    }))]]
  
  # Click on it
  webElem$clickElement()
  Sys.sleep(timeout)
  
  
  # Skip the appropriate number of articles
  
  skip_number <- i
  
  while (skip_number > 0) {
    webElem <- remDr$findElement("class name", "nextItem")
    webElem$clickElement()
    Sys.sleep(timeout)  
    skip_number <- skip_number - 1
  }
  
  # Update loop_status
  loop_status <- 
    tryCatch({remDr$findElement("class name", "nextItem"); TRUE},
             error = function(e) FALSE)
  
  # Select 100 articles
  webElem <- remDr$findElement("id", "selectAll")
  webElem$clickElement()
  Sys.sleep(timeout)
  
  ## Download articles
  
  # Click "eye" button
  webElem <- remDr$findElement("id", "enableppsview")
  webElem$clickElement()
  Sys.sleep(timeout)

  if (i != 0) readline("RESOLVE THE CAPTCHA THEN HIT ENTER")

  # Click "Display options"
  webElem <- remDr$findElement("class name", "pnlTabOpen")
  webElem$clickElement()
  Sys.sleep(timeout)
  
  # Click "Full article"
  webElem <- remDr$findElement("link text", "Full Article/Report plus Indexing")
  webElem$clickElement()
  Sys.sleep(timeout)
  
  # Save articles
  webElem <- remDr$findElement("class name", "ppssave")
  webElem$clickElement()
  Sys.sleep(timeout)
  
  # Click "Article format"
  webElem <- remDr$findElement("link text", "Article Format")
  webElem$clickElement()
  
  if (i != 0) readline("RESOLVE THE CAPTCHA THEN HIT ENTER")
  Sys.sleep(10)
  
  # Switch to window and download source
  remDr$switchToWindow(remDr$getWindowHandles()[[2]])
  articles <- remDr$getPageSource()
  write_file(articles[[1]], paste0("data/media_", cityname, "/FTV/", cityname, 
                                   "_", i, ".html"))
  rm(articles)
  Sys.sleep(timeout)
  
  # Close window and get back to main window
  remDr$closeWindow()
  remDr$switchToWindow(remDr$getWindowHandles()[[1]])
  Sys.sleep(timeout)
  
  i <- i + 1
  
  
}

upgo_scrape_disconnect()




