options(echo = TRUE)
print(sys.nframe())

# prevent running main code when sourced
if (sys.nframe() == 0) {
    # ... do main stuff
    print("main lines running (when script run in interactive mode)")
}


# & "C:\Program Files\R\R-4.2.2\bin\Rscript.exe" "C:\Users\Bakr\Documents\GitHub\Scrape-GSMArena-R\R_prevent_running_main_code_when_sourced.r"
## sys.nframe() == 0

# source("R_prevent_running_main_code_when_sourced.r")
## sys.nframe() == 4
