# bfcv - Basic Fortran CIF Viewer
A basic program capable of reading CIF files and visualizing the crystallogrpahic information they contain.

## Requirements
To compile this program a fortran compiler capable of fortran90 is needed, as well as the installation of [gtk-fortran](https://github.com/vmagnin/gtk-fortran). To run this program you need the gtk4 library.

## Compilation
To compile this program please make sure that all the requirements are installed and that your PKG_CONFIG_PATH points to the correct directories as explained in the guide of gtk-fortran. Once this is done simply run make:
```
make
```
