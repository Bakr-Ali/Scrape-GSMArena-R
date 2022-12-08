# scrape-gsma

Scrape the GSMArena website for collecting data of mobile devices

## Background

GSMArena is a gadget review website with a focus on cellular and mobile devices. This data is a labeled dataset extracted from the GSMArena webiste - one of the most popular online provider of phone information - and holds a large collection of phone specification.

## The Data

There are 116 unique phone brands and 10,000+ mobile phone models with 86 different specification fields.

## Extraction Method

The dataset was scraped using rvest, htmltab, and xml2 libraries in the R programming language.

## TODO

- [ ] Recreate the dataset with url ids and oem-device columns
- [ ] Scrape latest devices only
    - [ ] ~~Count oem devices in gsm.csv, compare it with number of devices in oems page~~
    - not reliable: xolo listed as 81, contains only 80. Some oems have 40 device/page, others 85 devicec/page.
- [ ] Reduce repeated code by refrencing functions from the OG file [Define all functions in one .R file, call them from another .R file. How, if possible?](https://stackoverflow.com/questions/13548266/define-all-functions-in-one-r-file-call-them-from-another-r-file-how-if-pos)
