fixColNames <- function(ss) {
    gsub(" ","_",ss) %>% gsub("_\\(.*\\)$","",.)
}

read_halo <- function(ff,...) {

    read_csv(ff,...) %>% rename_all(~fixColNames(.))

}

