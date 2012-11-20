.onLoad <- function(libname, pkgname)
{
  library.dynam("depthproc",pkgname,libname)
}

.onUnload <- function(libpath)
{
  library.dynam.unload("depthproc",libpath)
}