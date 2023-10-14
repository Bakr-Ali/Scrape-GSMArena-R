
## scraping gsmarena website ##


# test system sound
alert_sound <- function(x = 1, n = 1) {
  for (i in seq_len(n)) {
    if (.Platform$OS.type == "unix") {
      # system("spd-say 'get back to work'")
      system("tput bel")
      system("paplay /usr/share/sounds/gnome/default/alerts/sonar.ogg") # https://stackoverflow.com/a/3366068
    } else {
      # system("cmd.exe", input = "echo `a")
      system("powershell -c (New-Object Media.SoundPlayer 'C:/Windows/Media/notify.wav').PlaySync();")
    }
    Sys.sleep(x)
  }
}


# all of them from https://www.tidyverse.org/packages/ except "htmltab"
library(rvest)
library(dplyr)
library(magrittr)
library(htmltab)
library(purrr)
library(jsonlite)
library(stringr)

# setwd("git/scrape-gsma/")

options(stringsAsFactors = FALSE)


# don't run these vpn functions if not on Linux
switch_vpn <- function(x = 10) {
  if (.Platform$OS.type == "unix") {
    disconnect_vpn()
    print("Switching VPN server..")
    protonvpn_resp <- system("protonvpn-cli c -r", intern = TRUE)
    if (!grepl("Successfully connected", protonvpn_resp[4], fixed = TRUE)) {
      switch_vpn()
    }
    print("VPN changed!")
    Sys.sleep(x)
  } else {
    print("Not running on linux. No cli vpn.")
  }
}
disconnect_vpn <- function(x = 1) {
  if (.Platform$OS.type == "unix") {
    print("Disconnecting VPN..")
    protonvpn_resp <- system("protonvpn-cli d", intern = TRUE)
    print("VPN Disconnected!")
    Sys.sleep(x)
  } else {
    print("Not running on linux. No cli vpn.")
  }
}

safe_read_html <- function(url, x = 10) {
  switch_vpn(x)
  tryCatch(
    {
      print(paste("Getting html content of:", url))
      xml2::read_html(url)
    },
    
    error = function(e) {
      print(paste("ERROR Retry getting html of:", url))
      disconnect_vpn()
      safe_read_html(url, 30)
    }
  )
}

safe_download_xml <- function(url, x = 10) {
  switch_vpn(x)
  tryCatch(
    {
      print(paste("Downloading xml content of:", url))
      xml2::download_xml(url)
    },
    
    error = function(e) {
      print(paste("ERROR Retry downloading xml of:", url))
      disconnect_vpn()
      safe_download_xml(url, 30)
    }
  )
}


if (file.exists("gsm.csv")) {
  gsm <- read.csv("gsm.csv")
}

if (file.exists("./Data/oem_table.csv")) {
  old_oem_table <- read.csv("./Data/oem_table.csv")
}


build_oem_table <- function(...) {
  
  # TODO: VPN here?
  sesh <- session("https://www.gsmarena.com/makers.php3")
  makers <- safe_read_html(sesh)
  
  maker_nodes <- makers %>% html_nodes(".st-text a")
  # <a href="acer-phones-59.php">Acer<br><span>100 devices</span></a>
  # <a href="alcatel-phones-5.php">alcatel<br><span>407 devices</span></a>
  
  maker_names <- maker_nodes %>% html_text()
  # chr [1:119]
  # "Acer100devices"
  # "alcatel407 devices"
  
  maker_devices_count <- makers %>% html_nodes(".st-text span") %>% html_text()
  # 
  # "100 devices"
  # "407 devices"
  
  oem_names <- mapply(gsub, maker_devices_count, "", maker_names) %>% `names<-`(NULL)
  # chr [1:119]
  # "Acer"
  # "alcatel"
  
  maker_devices_count <- gsub(pattern = " devices", replacement = "", x = maker_devices_count) %>% as.numeric()
  # num [1:119]
  # 100
  # 407
  
  maker_resource_location = maker_nodes %>% html_attr("href")
  # chr [1:119]
  # "acer-phones-59.php"
  # "alcatel-phones-5.php"
  
  oem_table <- data.frame(maker = oem_names, device_count = maker_devices_count, resource_location = maker_resource_location)
  #               maker device_count             resource_location
  # 1              Acer          100            acer-phones-59.php
  # 2           alcatel          407          alcatel-phones-5.php
  
  # https://stackoverflow.com/questions/60791284/why-does-stringrstr-match-on-a-column-return-a-matrix
  oem_table <- oem_table %>% mutate(maker_url = paste0("https://www.gsmarena.com/", oem_table$resource_location))
  
  oem_table <- oem_table %>% mutate(maker_id = stringr::str_match(oem_table$maker_url, "https://www.gsmarena.com/(.*?)-phones-")[,2])
  
  oem_table <- oem_table %>% mutate(maker_indx = stringr::str_match(oem_table$maker_url, ".*-phones-(.*?).php")[,2])
  
  # oem_table <- oem_table %>% mutate(new_devices_count = oem_table$device_count - old_oem_table$device_count)
  
  # oem_table <- oem_table %>% mutate(new_devices_count = oem_table$device_count[oem_table$resource_location] - old_oem_table$device_count[old_oem_table$resource_location])
  
  # subtract new device count from old one to find number of devices added since last scrape
  for (i in seq_len(nrow(oem_table))) {
    tryCatch(
      {
        # This could error out (when there are new oems), that's why we use trycatch
        oem_table$number_of_new[i] <- oem_table$device_count[i] - old_oem_table$device_count[which(old_oem_table$resource_location == oem_table$resource_location[i])]
      },
      
      error = function(e) {
        if (grepl("replacement has length zero", as.character(e), fixed = TRUE)) {
          oem_table$number_of_new[i] <- oem_table$device_count[i] - 0
        } else {
          e
        }
      }
    )
  }
  
  file.copy("./Data/oem_table.csv", "./Data/old_oem_table.csv", overwrite = TRUE)
  
  write.csv(oem_table, file = "./Data/oem_table.csv")
  
  return(oem_table)
}

# oem_table <- build_oem_table()


parse_resource_locator <- function(location) {
  paste0("https://www.gsmarena.com/", location)
  # "https://www.gsmarena.com/acer-phones-59.php"
  # "https://www.gsmarena.com/alcatel-phones-5.php"
  # "https://www.gsmarena.com/samsung-phones-9.php"
}


oem_urls <- function(oem_base_url) {
  ##oem_base_url <- "https://www.gsmarena.com/samsung-phones-9.php" ###
  
  src <- safe_read_html(oem_base_url)
  Sys.sleep(3)
  
  items <- src %>% html_nodes(".nav-pages strong , .nav-pages a") %>% html_text()
  # chr [1:16]
  # "1"
  # "2"
  
  if (length(items) != 0) {
    page_range <- 1:(items[length(items)] %>% as.numeric())
    # int [1:16]
    # 1
    # 2
    
    # TODO Remove these from here and source them from build_oem_table file/function
    maker_id <- stringr::str_match(oem_base_url, "https://www.gsmarena.com/(.*?)-phones-")[2]
    # "samsung"
    maker_indx <- stringr::str_match(oem_base_url, ".*-phones-(.*?).php")[2]
    # "9"
    
    map_chr(page_range, 
            function(pg_count) {
              paste0("https://www.gsmarena.com/", maker_id, "-phones-f-",
                     maker_indx, "-0-p", pg_count, ".php"
              )
            }
    )
    # "https://www.gsmarena.com/samsung-phones-f-9-0-p1.php"
    # "https://www.gsmarena.com/samsung-phones-f-9-0-p2.php"
  } else {
    oem_base_url
  }
}

# oem_urls("https://www.gsmarena.com/samsung-phones-9.php")


listed_devices <- function(page_url) {
  ## page_url <- "https://www.gsmarena.com/samsung-phones-f-9-0-p1.php" ###
  
  src <- safe_read_html(page_url)
  nodes <- src %>% html_nodes("#review-body a")
  # <a href="samsung_galaxy_a04e-11945.php"><img src="https://fdn2.gsmarena.com/vv/bigpic/samsung-galaxy-a04e.jpg" title="Samsung Galaxy A04e Android smartphone. Announced Oct 2022. Features 6.5″  display, 5000 mAh battery, 128 GB storage, 4 GB RAM."><strong><span>Galaxy A04e</span></strong></a>
  
  devices <- nodes %>% html_text()
  # chr [1:85]
  # "Galaxy A04e"
  # "Galaxy Tab Active4 Pro"
  # "Galaxy A04s"
  devices_url <- nodes %>% html_attr("href")
  # chr [1:85]
  # "samsung_galaxy_a04e-11945.php"
  # "samsung_galaxy_tab_active4_pro-11840.php"
  # "samsung_galaxy_a04s-11803.php"
  
  data.frame(device_name = devices, device_resource = devices_url)
  #                  device_name                             device_resource
  # 1                Galaxy A04e               samsung_galaxy_a04e-11945.php
  # 2     Galaxy Tab Active4 Pro    samsung_galaxy_tab_active4_pro-11840.php
  # 3                Galaxy A04s               samsung_galaxy_a04s-11803.php
}

# listed_devices("https://www.gsmarena.com/samsung-phones-f-9-0-p1.php")


scrape_df <- function(url) {
  ## url <- "https://www.gsmarena.com/samsung_galaxy_a04e-11945.php" ###
  
  src <- safe_read_html(url)
  switch_vpn()
  doc <- safe_download_xml(url)
  # file "samsung_galaxy_a04e-11945.php" ???
  
  # `xml2::download_xml(url)` is almost identical to browser source code.
  # Both read functions convert the encoding to UTF-8 and they are almost identical.
  
  # number of [sub]tables on page
  n_head <- src %>% html_nodes("th") %>% length()
  # 13(L??)
  # TODO explore changing ~~`safe_read_html(url)`~~ `src` with `doc`
  
  get_head_tbl <- function(head_indx) {
    ## head_indx <- 3 ### 1:13L
    
    out = tryCatch(
      {
        suppressMessages(htmltab(doc, which = head_indx, rm_nodata_cols = FALSE) %>%
                           #   Body Dimensions               164.2 x 75.9 x 9.1 mm (6.46 x 2.99 x 0.36 in)
                           # 2 Body     Weight                                             188 g (6.63 oz)
                           # 3 Body      Build                    Glass front, plastic back, plastic frame
                           # 4 Body        SIM Single SIM (Nano-SIM) or Dual SIM (Nano-SIM, dual stand-by)
                           as.data.frame() %>%
                           # same
                           rbind(colnames(.), .) %>%
                           #   Body Dimensions               164.2 x 75.9 x 9.1 mm (6.46 x 2.99 x 0.36 in)
                           # 1 Body Dimensions               164.2 x 75.9 x 9.1 mm (6.46 x 2.99 x 0.36 in)
                           # 2 Body     Weight                                             188 g (6.63 oz)
                           # 3 Body      Build                    Glass front, plastic back, plastic frame
                           # 4 Body        SIM Single SIM (Nano-SIM) or Dual SIM (Nano-SIM, dual stand-by)
                           `colnames<-`(c("type", "sub_type", "val")))
        #   type   sub_type                                                         val
        # 1 Body Dimensions               164.2 x 75.9 x 9.1 mm (6.46 x 2.99 x 0.36 in)
        # 2 Body     Weight                                             188 g (6.63 oz)
        # 3 Body      Build                    Glass front, plastic back, plastic frame
        # 4 Body        SIM Single SIM (Nano-SIM) or Dual SIM (Nano-SIM, dual stand-by)
      },
      
      error = function(e) {
        xp <- '//th | //*[contains(concat( " ", @class, " " ), concat( " ", "ttl", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "nfo", " " ))]'
        print(paste("Fetching chunk", head_indx, "of", n_head))
        # "Fetching chunk 3 of 13"
        
        # TODO this is not working, getting the following error.
          # Error in XML::htmlParse("", list(encoding = "UTF-8")) : empty or no content specified
        # Try instead:
          # replace `url` with `toString(src)`
        switch_vpn()
        suppressMessages(htmltab(url, which = head_indx, body = xp) %>%
                           as.data.frame() %>%
                           rbind(colnames(.), .) %>%
                           `colnames<-`(c("type", "sub_type", "val")))
      }
    )
    
    out
    
  }
  
  df <- map(1:n_head, get_head_tbl) %>% bind_rows()
  #             type    sub_type                                                                   val
  # 1        Network  Technology                                                      GSM / HSPA / LTE
  # 2        Network    2G bands     GSM 850 / 900 / 1800 / 1900 - SIM 1 & SIM 2 (dual-SIM model only)
  # 3        Network    3G bands                                                HSDPA 850 / 900 / 2100
  # 4        Network    4G bands                                     1, 3, 5, 7, 8, 20, 28, 38, 40, 41
  # 5        Network       Speed                                                             HSPA, LTE
  # 6         Launch   Announced                                                      2022, October 21
  # 7         Launch      Status                                 Available. Released 2022, November 07
  # 8           Body  Dimensions                         164.2 x 75.9 x 9.1 mm (6.46 x 2.99 x 0.36 in)
  # 9           Body      Weight                                                       188 g (6.63 oz)
  # 10          Body       Build                              Glass front, plastic back, plastic frame
  # 11          Body         SIM           Single SIM (Nano-SIM) or Dual SIM (Nano-SIM, dual stand-by)
  # 12       Display        Type                                                               PLS LCD
  # 13       Display        Size                    6.5 inches, 102.0 cm (~81.8% screen-to-body ratio)
  # 14       Display  Resolution                      720 x 1600 pixels, 20:9 ratio (~270 ppi density)
  # 15      Platform          OS                                           Android 12, One UI Core 4.1
  # 16      Platform         CPU                                         Octa-core (2.3 GHz & 1.8 GHz)
  # 17        Memory   Card slot                                            microSDXC (dedicated slot)
  # 18        Memory    Internal 32GB 3GB RAM, 32GB 4GB RAM, 64GB 3GB RAM, 64GB 4GB RAM, 128GB 4GB RAM
  # 19   Main Camera        Dual                        13 MP, f/2.2, (wide), AF  2 MP, f/2.4, (depth)
  # 20   Main Camera    Features                                                             LED flash
  # 21   Main Camera       Video                                                           1080p@30fps
  # 22 Selfie camera      Single                                                           5 MP, f/2.2
  # 23 Selfie camera       Video                                                                   Yes
  # 24         Sound Loudspeaker                                                                   Yes
  # 25         Sound  3.5mm jack                                                                   Yes
  # 26         Comms        WLAN                                      Wi-Fi 802.11 b/g/n, Wi-Fi Direct
  # 27         Comms   Bluetooth                                                         5.0, A2DP, LE
  # 28         Comms Positioning                                            GPS, GLONASS, GALILEO, BDS
  # 29         Comms         NFC                                                                    No
  # 30         Comms       Radio                                                           Unspecified
  # 31         Comms         USB                                         USB Type-C 2.0, USB On-The-Go
  # 32      Features     Sensors                                              Accelerometer, proximity
  # 33       Battery        Type                                         Li-Po 5000 mAh, non-removable
  # 34          Misc      Colors                                             Black, Copper, Light Blue
  # 35          Misc      Models                          SM-A042F, SM-A042F/DS, SM-A042M, SM-A042M/DS
  # 36          Misc      SAR EU                             0.29 W/kg (head)     1.11 W/kg (body)    
  # 37          Misc       Price                                                              £ 134.99
  
  # deletes the downloaded .php files
  system("rm *.php")
  df
}


safe_scraper <- safely(scrape_df, otherwise = NULL)


ll <- list(devices = list())
#  $ devices: list()


loop_the_loop <- function() {
  
  if (exists("oem_table")) {
    
    print("oem table exists")
    
  } else {
    
    print("building oem table...")
    
    oem_table <<- build_oem_table()
    
    print("oem table built!")
    
  }
  # oem_table:
  # maker (chr), device_count (num), resource_location (chr)
  # "Acer", 100, "acer-phones-59.php"
  # "alcatel", 407, "alcatel-phones-5.php"
  # "Samsung", 1343, "samsung-phones-9.php"
  
  #               maker device_count             resource_location
  # 1              Acer          100            acer-phones-59.php
  # 2           alcatel          407          alcatel-phones-5.php
  
  for (oem in oem_table$maker[1:nrow(oem_table)]) {
    ## oem <- "Samsung" ###
    print(paste("processing OEM:", oem))
    # "processing OEM: Samsung"
    
    oem_listings <- parse_resource_locator(oem_table$resource_location[oem_table$maker == oem]) %>% oem_urls()
    # chr [1:16]
    # "https://www.gsmarena.com/samsung-phones-f-9-0-p1.php"
    # "https://www.gsmarena.com/samsung-phones-f-9-0-p2.php"
    
    print(paste("Pages found:", length(oem_listings)))
    # "Pages found: 16"
    
    ## ll$devices[[oem]] <- list() ###
    ll$devices[[oem]] <<- list()
    #  $ devices:List of 1
    #   ..$ Samsung: list()
    
    for (page in oem_listings) {
      # page <- "https://www.gsmarena.com/samsung-phones-f-9-0-p1.php"
      devices_on_page <- listed_devices(page)
      # device_name (chr), device_resource (chr)
      
      #            device_name (chr)                       device_resource (chr)
      # 1                Galaxy A04e               samsung_galaxy_a04e-11945.php
      # 2     Galaxy Tab Active4 Pro    samsung_galaxy_tab_active4_pro-11840.php
      # 3                Galaxy A04s               samsung_galaxy_a04s-11803.php
      
      for (device in devices_on_page$device_name) {
        # device <- "Galaxy A04e"
        
        if (device %in% gsm$model && oem %in% gsm$oem) {
          # TODO this is (probably?) wrong, correct it by adding a "oem_and_model" 
          # column to the gsm.csv data and checking with:
          # if (paste(oem, device) %in% gsm$oem_and_model)
          
          print("device exists. skipping...")
          
        } else {
          
          print(paste("retrieving data for:", device))
          
          out = tryCatch(
            {
              gsm_data <- safe_scraper(devices_on_page %>%
                                         filter(device_name == device) %>%
                                         select(device_resource) %>% parse_resource_locator()
              )
              # safe_scraper("https://www.gsmarena.com/samsung_galaxy_a04e-11945.php")
              # 
              
              if (!is.null(gsm_data$result)) {
                
                gsm_data <- gsm_data$result
                tmp_df <- data.frame(type = c("oem", "model"), sub_type = c("", ""), val = c(oem, device))
                #    type sub_type         val
                # 1   oem              Samsung
                # 2 model          Galaxy A04e
                
                # TODO add other columns above
                  # `oem - model`
                  # `resource_locator`?
                  # URL too?
                
                gsm_data <- rbind(tmp_df, gsm_data)
                # joins them
                
                ## ll$devices[[oem]][[device]] <- gsm_data ###
                ll$devices[[oem]][[device]] <<- gsm_data
                
                # TODO move this outside the loop because it overwrites the .json file with each loop
                writeLines(toJSON(ll), "gsm.json")
              }
            }
          )
        }
      }
    }
  }
}


loop_the_loop()


new_data_json <- readLines("gsm.json") %>% fromJSON()


long_to_wide <- function(df) {
  as.data.frame(t(df$val)) %>%
    `colnames<-`(paste0(df$type, "_", df$sub_type) %>%
                   str_trim(side = "both"))
}


gsm_new_devices <- new_data_json$devices %>% purrr::flatten() %>% map(long_to_wide) %>% bind_rows()


# remove trailing underscores from col names, replace spaces with underscores, lowercase col names

colnames(gsm_new_devices) <- colnames(gsm_new_devices) %>% str_replace("_\\Z", "") %>%
  str_replace_all(" ", "_") %>%  str_trim() %>% tolower()

# what is this for?
colnames(gsm_new_devices) <- colnames(gsm_new_devices) %>% str_replace("_na", "") %>%
  str_replace("\\.\\.\\.[0-9]+", "")


df <- bind_rows(gsm_new_devices, gsm)

write.csv(df, "gsm.csv", na = "", row.names = FALSE)
