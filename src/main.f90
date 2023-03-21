! This program reads in a file describing a molecule and calculates the center of mass
program bfcv
    use gtk_application
    implicit none

    ! Start application
    call gtk_init_app()
    call gtk_deinit_app()

end program bfcv
