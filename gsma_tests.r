
## scraping gsmarena website ##


# proxy settings

# Sys.setenv(http_proxy = "socks5://localhost:8888")
# Sys.setenv(HTTPS_PROXY = "socks5://localhost:8888")

if (.Platform$OS.type == "unix") {
  # system("spd-say 'get back to work'")
  system("tput bel")
} else {
  # system("cmd.exe", input = "echo `a")
  system("powershell -c (New-Object Media.SoundPlayer 'C:/Windows/Media/notify.wav').PlaySync();")
}

library(rvest)
library(dplyr)
library(magrittr)
library(htmltab)
library(purrr)
library(jsonlite)
library(stringr)

# setwd("git/scrape-gsma/")

options(stringsAsFactors = FALSE)


if (file.exists("gsm.csv")) {
  gsm <- read.csv("gsm.csv")
}


build_oem_table <- function(...) {
  
  sesh <- session("https://www.gsmarena.com/makers.php3")
  makers <- read_html(sesh)
  
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
  
  maker_url = maker_nodes %>% html_attr("href")
  # chr [1:119]
  # "acer-phones-59.php"
  # "alcatel-phones-5.php"
  
  oem_table <- data.frame(maker = oem_names, device_count = maker_devices_count, resource_location = maker_url)
  #               maker device_count             resource_location
  # 1              Acer          100            acer-phones-59.php
  # 2           alcatel          407          alcatel-phones-5.php
  
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
  src <- read_html(oem_base_url); Sys.sleep(3)
  
  items <- src %>% html_nodes(".nav-pages strong , .nav-pages a") %>% html_text()
  # chr [1:16]
  # "1"
  # "2"
  
  if (length(items) != 0) {
    page_range <- 1:(items[length(items)] %>% as.numeric())
    # int [1:16]
    # 1
    # 2
    
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
  
  src <- read_html(page_url)
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
  
  # src <- read_html(url)
  doc <- xml2::download_xml(url)
  # file "samsung_galaxy_a04e-11945.php" ???
  
  # `xml2::download_xml(url)` is almost identical to browser source code.
  # Both read functions convert the encoding to UTF-8 and they are almost identical.
  
  # number of [sub]tables on page
  n_head <- xml2::read_html(url) %>% html_nodes("th") %>% length()
  # 13(L??)
  # TODO explore changing `xml2::read_html(url)` with `doc`
  
  get_head_tbl <- function(head_indx) {
    
    ## head_indx <- 2 ### 1:13L
    out = tryCatch(
      {
        suppressMessages(htmltab(doc, which = head_indx, rm_nodata_cols = FALSE) %>%
                           as.data.frame() %>%
                           rbind(colnames(.), .) %>%
                           `colnames<-`(c("type", "sub_type", "val")))
      },
      
      error = function(e) {
        xp <- '//th | //*[contains(concat( " ", @class, " " ), concat( " ", "ttl", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "nfo", " " ))]'
        print(paste("Fetching chunk", head_indx, "of", n_head))
        
        suppressMessages(htmltab(url, which = head_indx, body = xp) %>%
                           as.data.frame() %>%
                           rbind(colnames(.), .) %>%
                           `colnames<-`(c("type", "sub_type", "val")))
      }
    )
    
    out
    
  }
  
  df <- map(1:n_head, get_head_tbl) %>% bind_rows()
  
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
          # TODO this is wrong, correct it by adding a "oem_and_model" column to 
          # the gsm.csv data and checking with:
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
                
                gsm_data <- rbind(tmp_df, gsm_data)
                
                ll$devices[[oem]][[device]] <<- gsm_data
                
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

colnames(gsm_new_devices) <- colnames(gsm_new_devices) %>% str_replace("_na", "") %>%
  str_replace("\\.\\.\\.[0-9]+", "")


df <- bind_rows(gsm_new_devices, gsm)

write.csv(df, "gsm.csv", na = "", row.names = FALSE)
