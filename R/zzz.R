
.onLoad <- function(libname, pkgname) {

  # build python enviroment
  reticulate::py_require(packages = c("anndata",
                                      "pandas",
                                      "pyarrow"),
                         python_version = "3.12")

  # load utils_anndata_python.py
  py_file <- system.file("Python",
                         "utils_anndata_python.py",
                         package = "AdataPangoR")
  reticulate::source_python(py_file)

  }
