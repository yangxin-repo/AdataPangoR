
#' the S4 class of AdataPangoR
#'
#' @slot h5ad_path the path of h5ad file
#' @slot adata_obj the python anndata object
#' @export
setClass(Class = "AdataPangoR",
         slots = list(
           h5ad_path="character",
           adata_obj="ANY"
         ))

#' the create function of AdataPangoR
#'
#' @param h5ad the path of h5ad file
#' @export
AdataPangoR <- function(h5ad_path){

  ad <- reticulate::import("anndata")

  new(Class = "AdataPangoR",
      h5ad_path = h5ad_path,
      adata_obj = ad$read_h5ad(h5ad_path))

}

#' the generic of get_adata_obs_pango
#' @export
setGeneric(name = "get_adata_obs_pango",
           def = function(adata,
                          ...){
             standardGeneric("get_adata_obs_pango")
           })

#' the get_adata_obs_pango method for AdataPangoR
#'
#' @export
setMethod(f = "get_adata_obs_pango",
          signature = signature(adata="AdataPangoR"),
          definition = function(adata,
                                ...){

            tmp <- reticulate::py_get_attr(adata@adata_obj, "obs") |>
              reticulate::py$get_dataframe_as_parquet()

            obs_r <- nanoparquet::read_parquet(tmp) |>
              tibble::as_tibble()

            unlink(tmp)

            return(obs_r)

          })

#' the generic of get_adata_obsm_pango
#' @export
setGeneric(name = "get_adata_obsm_pango",
           def = function(adata,
                          obsm_key,
                          ...){
             standardGeneric("get_adata_obsm_pango")
           })

#' the get_adata_obsm_pango method for AdataPangoR
#'
#' @export
setMethod(f = "get_adata_obsm_pango",
          signature = signature(adata="AdataPangoR",
                                obsm_key="character"),
          definition = function(adata,
                                obsm_key,
                                ...){

            if (obsm_key=="spatial") {

              obsm_r <- adata@adata_obj$obsm[obsm_key]
              obsm_r <- tibble::tibble(x = obsm_r[, 1],
                                       y = obsm_r[, 2])

            }

            obs_names_r <- get_adata_obs_names_pango(adata)

            obsm_r <- dplyr::bind_cols(obs_names_r,obsm_r)

            return(obsm_r)

          })

#' the generic of get_adata_obs_names_pango
#' @export
setGeneric(name = "get_adata_obs_names_pango",
           def = function(adata,
                          ...){
             standardGeneric("get_adata_obs_names_pango")
           })

#' the get_adata_obs_names_pango method for AdataPangoR
#'
#' @export
setMethod(f = "get_adata_obs_names_pango",
          signature = signature(adata="AdataPangoR"),
          definition = function(adata,
                                ...){

            tmp <- reticulate::py_get_attr(adata@adata_obj, "obs_names") |>
              reticulate::py$get_index_as_parquet()

            obs_names_r <- nanoparquet::read_parquet(tmp) |>
              tibble::as_tibble() |>
              dplyr::rename(obs_names=index)

            unlink(tmp)

            return(obs_names_r)

          })
