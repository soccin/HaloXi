fixColNames <- function(ss) {
    gsub(" ","_",ss) |> gsub("_\\(.*\\)$","",x=_)
}

read_halo <- function(ff,...) {

    readr::read_csv(ff, show_col_types = FALSE, progress=F,...) |> dplyr::rename_all(~fixColNames(.))

}

