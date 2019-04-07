.onAttach <- function (libname,pkgname){
  packageStartupMessage('Welcome to tsdo package!');

}

.onLoad <- function (libname,pkgname)
{
  print('load finished!')
}
