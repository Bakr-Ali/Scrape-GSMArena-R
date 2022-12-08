
## scraping gsmarena website ##


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


# TODO: don't run these vpn functions if not on Linux
switch_vpn <- function(x = 10) {
  print("Switching VPN server..")
  protonvpn_resp <- system("protonvpn-cli c -r", intern = TRUE)
  if (!grepl("Successfully connected", protonvpn_resp[4], fixed = TRUE)) {
    disconnect_vpn(5)
    switch_vpn()
  }
  print("VPN changed!")
  Sys.sleep(x)
}
disconnect_vpn <- function(x = 1) {
  print("Disconnecting VPN..")
  protonvpn_resp <- system("protonvpn-cli d", intern = TRUE)
  print("VPN Disconnected!")
  Sys.sleep(x)
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


if (file.exists("gsm.csv")) {
  gsm <- read.csv("gsm.csv")
}


build_oem_table <- function(...) {
  
  # TODO: VPN here?
  sesh <- session("https://www.gsmarena.com/makers.php3")
  makers <- safe_read_html(sesh)
  
  maker_nodes <- makers %>% html_nodes(".st-text a")
  
  maker_names <- maker_nodes %>% html_text()
  
  maker_devices_count <- makers %>% html_nodes(".st-text span") %>% html_text()
  
  oem_names <- mapply(gsub, maker_devices_count, "", maker_names) %>% `names<-`(NULL)
  
  maker_devices_count <- gsub(pattern = " devices", replacement = "", x = maker_devices_count) %>% as.numeric()
  
  maker_url = maker_nodes %>% html_attr("href")
  
  oem_table <- data.frame(maker = oem_names, device_count = maker_devices_count, resource_location = maker_url)
  
  return(oem_table)
}

# oem_table <- build_oem_table()


parse_resource_locator <- function(location) {
  paste0("https://www.gsmarena.com/", location)
}


oem_urls <- function(oem_base_url) {
  src <- safe_read_html(oem_base_url)
  Sys.sleep(3)
  
  items <- src %>% html_nodes(".nav-pages strong , .nav-pages a") %>% html_text()
  
  if (length(items) != 0) {
    page_range <- 1:(items[length(items)] %>% as.numeric())
    
    maker_id <- stringr::str_match(oem_base_url, "https://www.gsmarena.com/(.*?)-phones-")[2]
    maker_indx <- stringr::str_match(oem_base_url, ".*-phones-(.*?).php")[2]
    
    map_chr(page_range, 
            function(pg_count) {
              paste0("https://www.gsmarena.com/", maker_id, "-phones-f-",
                     maker_indx, "-0-p", pg_count, ".php"
              )
            }
    )
  } else {
    oem_base_url
  }
}

# oem_urls("https://www.gsmarena.com/samsung-phones-9.php")


listed_devices <- function(page_url) {
  
  src <- safe_read_html(page_url)
  nodes <- src %>% html_nodes("#review-body a")
  
  devices <- nodes %>% html_text()
  devices_url <- nodes %>% html_attr("href")
  
  data.frame(device_name = devices, device_resource = devices_url)
}

# listed_devices("https://www.gsmarena.com/samsung-phones-f-9-0-p1.php")


scrape_df <- function(url) {
  
  src <- safe_read_html(url)
  switch_vpn()
  doc <- xml2::download_xml(url)
  
  # number of [sub]tables on page
  n_head <- src %>% html_nodes("th") %>% length()
  # TODO explore changing ~~`safe_read_html(url)`~~ `src` with `doc`
  
  get_head_tbl <- function(head_indx) {
    
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
  
  system("rm *.php")
  df
}


safe_scraper <- safely(scrape_df, otherwise = NULL)


ll <- list(devices = list())


loop_the_loop <- function() {
  
  if (exists("oem_table")) {
    
    print("oem table exists")
    
  } else {
    
    print("building oem table...")
    
    oem_table <<- build_oem_table()
    
    print("oem table built!")
    
  }
  
  for (oem in oem_table$maker[1:nrow(oem_table)]) {
    print(paste("processing OEM:", oem))
    
    oem_listings <- parse_resource_locator(oem_table$resource_location[oem_table$maker == oem]) %>% oem_urls()
    
    print(paste("Pages found:", length(oem_listings)))
    
    ll$devices[[oem]] <<- list()
    
    for (page in oem_listings) {
      devices_on_page <- listed_devices(page)
      
      for (device in devices_on_page$device_name) {
        
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
              
              if (!is.null(gsm_data$result)) {
                
                gsm_data <- gsm_data$result
                tmp_df <- data.frame(type = c("oem", "model"), sub_type = c("", ""), val = c(oem, device))
                # TODO add other columns above
                  # `oem - model`
                  # `resource_locator`?
                  # URL too?
                
                gsm_data <- rbind(tmp_df, gsm_data)
                
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
