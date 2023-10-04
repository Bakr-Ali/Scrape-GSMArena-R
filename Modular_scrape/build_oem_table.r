library(rvest)

source(safe_read_html)

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
