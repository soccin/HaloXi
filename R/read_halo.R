fixColNames <- function(ss) {
    ## Halo exports vary in encoding (some use Latin-1 for the um/squared
    ## glyphs in the area columns), which breaks gsub's UTF-8 handling;
    ## sanitise to valid UTF-8 first.
    ss <- enc2utf8(iconv(ss, from = "", to = "UTF-8", sub = "byte"))
    gsub(" ","_",ss) |> gsub("_\\(.*\\)$","",x=_)
}

read_halo <- function(ff,...) {

    readr::read_csv(ff, show_col_types = FALSE, progress=F,...) |> dplyr::rename_all(~fixColNames(.))

}

