
url <- "https://www.gsmarena.com/samsung_galaxy_a04e-11945.php"

xml2::write_xml(read_html(url), file = "compare_html_downloads/read_html_0.html")
xml2::write_xml(xml2::read_html(url), file = "compare_html_downloads/read_html_xml2.html")
xml2::write_xml(rvest::read_html(url), file = "compare_html_downloads/read_html_rvest.html")
xml2::download_xml(url)


src <- xml2::read_html(url)

my_save_html <- function(l_src, file_name) {
  file_conn <- file(file_name)
  writeLines(l_src, file_conn) # TODO can use direct file name instead https://stackoverflow.com/a/50964907/9177407
  close(file_conn)
}

my_save_html(as.character(src), "read_html_xml2-write_lines_as_character.html")
# my_save_html(toString(src), "read_html_xml2-write_lines_toString.html") # exactly the same as `as.char`

# https://stackoverflow.com/a/55142671/9177407
  # cat(src, file = "output.html") # argument 1 (type 'list') cannot be handled by 'cat'
  # solution: https://stackoverflow.com/questions/52848756/using-cat-function-on-data-type-list
  # paste0(unlist(src), collapse = "\n") # "<pointer: 0x0000015dc6e50fa0>\n<pointer: 0x0000015dc1bf40f0>"
  # cat(content(GET(url), "text"), file = "output.html") # output blank <EMPTY BODY>
