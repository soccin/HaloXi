####################################################################################
args=commandArgs(trailing=T)
if(len(args)<1) {
    cat("\n")
    cat("   usage: loadHaloTables.R halo01.csv [halo02.csv]\n")
    cat("\n")
    cat("      - halo01.csv, halo02.csv, ... filenames of halo files to scan\n")
    cat("                                    can be compressed (.gz)\n")
    cat("\n")
    quit()
}
names(args)=args
####################################################################################
VERSION="v1.0.1"
SDIR=dirname(ndsTools::thisFile())
source(file.path(SDIR,"tools.R"))
####################################################################################
DATE <- function(){gsub("-", "", Sys.Date())}
####################################################################################
require(tidyverse)
####################################################################################

dx=map(args,read_csv) %>% map(mutate,Sample=gsub(".tif","",`Image File Name`)) %>% bind_rows

