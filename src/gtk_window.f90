! Module which provides functions and subroutine to create, interact with and control a gtk application
module gtk_application
    use atoms
    use cif
    use gtk
    use gtk_sup
    use gtk_hl_chooser
    use g
    use, intrinsic :: iso_c_binding
    implicit none

    ! Visibility
    private
    public gtk_init_app
    public gtk_deinit_app

    ! Module variables
    type(c_ptr) :: gtk_app = c_null_ptr
    type(c_ptr) :: window = c_null_ptr
    type(c_ptr) :: menu_bar = c_null_ptr
    integer(c_int) :: status = 0
    character(len=16), dimension(1) :: filters = ["*.cif"], filter_names = ["CIF files"]

contains
    ! Initializes the application
    subroutine gtk_init_app()
        ! Create gtk app
        gtk_app = gtk_application_new("bfcv.gtk"//c_null_char, G_APPLICATION_FLAGS_NONE)
        call g_signal_connect(gtk_app, "activate"//c_null_char, c_funloc(window_activate))
        
        ! Run gtk app
        status = g_application_run(gtk_app, 0_c_int, [c_null_ptr])
    end subroutine

    ! Deinitializes the application
    subroutine gtk_deinit_app()
        call g_application_quit(gtk_app)
        call g_object_unref(window)
    end subroutine

    ! Initializes the window and its contents
    subroutine window_activate(app, gdata) bind(c)
        type(c_ptr), value, intent(in) :: app, gdata
        type(c_ptr) :: section = c_null_ptr
        type(c_ptr) :: menu = c_null_ptr, menu_item_open = c_null_ptr, menu_item_quit = c_null_ptr
        type(c_ptr) :: act_quit = c_null_ptr, act_open = c_null_ptr

        ! Create the window
        window = gtk_application_window_new(app)
        call gtk_window_set_title(window, "Basic Fortran CIF Viewer"//c_null_char)
        call gtk_window_set_default_size(window, 480, 640)

        ! Create menu bar
        menu_bar = g_menu_new()
        menu = g_menu_new()
        section = g_menu_new()
        menu_item_open = g_menu_item_new("Open File"//c_null_char, "app.open_file"//c_null_char)
        menu_item_quit = g_menu_item_new("Quit"//c_null_char, "app.quit"//c_null_char)

        ! Specify button actions
        act_quit = g_simple_action_new("quit"//c_null_char, c_null_ptr)
        call g_signal_connect(act_quit, "activate"//c_null_char, c_funloc(quit), gtk_app)
        call g_action_map_add_action(gtk_app, act_quit)
        act_open = g_simple_action_new("open_file"//c_null_char, c_null_ptr)
        call g_signal_connect(act_open, "activate"//c_null_char, c_funloc(open_file), gtk_app)
        call g_action_map_add_action(gtk_app, act_open)

        ! Add buttons to menu bar
        call g_menu_append_item(section, menu_item_open)
        call g_menu_append_item(section, menu_item_quit)
        call g_menu_append_section(menu, "@-->--->---"//c_null_char, section)
        call g_menu_append_submenu(menu_bar, "Menu"//c_null_char, menu)

        ! Deallocate used memory
        call g_object_unref(menu_item_open)
        call g_object_unref(menu_item_quit)

        ! Set the menubar and display it
        call gtk_application_set_menubar(app, menu_bar)
        call gtk_application_window_set_show_menubar(window, TRUE)

        ! Display the window
        call gtk_widget_show(window)
        call gtk_window_present(window)
    end subroutine

    ! Called when the 'quit' button is pressed, exits the application
    subroutine quit(act, param, win) bind(c)
        type(c_ptr), value, intent(in) :: act, param, win
        call gtk_deinit_app()
    end subroutine

    ! Called when the 'Open' button is pressed, opens a .cif file
    subroutine open_file(act, param, win) bind(c)
        type(c_ptr), value, intent(in) :: act, param, win
        type(c_ptr) :: file_chooser = c_null_ptr
        integer(c_int) :: ret_val
        character(len=256), dimension(:), allocatable :: selected
        character(len=256) :: file_name
        type(atom), allocatable :: atom_list(:)

        ! Ask for file
        ret_val = hl_gtk_file_chooser_show(selected, create=FALSE, title="Select one .cif file"//c_null_char, &
            filter=filters, filter_name = filter_names, edit_filters=TRUE, parent=window, all=TRUE)

        ! Check if a file was selected
        print *, ret_val
        if (ret_val == FALSE) return

        ! Open the .cif file and extract the atoms
        file_name = selected(1)
        deallocate(selected)
        atom_list = cif_extract_atoms(file_name)
        call print_atoms(atom_list)

        !ret_val = gtk_dialog_run(file_chooser)
        !print *, ret_val
    end subroutine

end module gtk_application
