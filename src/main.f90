! Basic Fortran CIF Viewer
program bfcv
    use gtk_application
    implicit none

    ! Start application
    call gtk_init_app()
    call gtk_deinit_app()

end program bfcv
