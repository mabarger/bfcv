! Module which provides functions and subroutine to create, interact with and control a gtk application
module gtk_application
    use atoms
    use cairo
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
    integer, parameter :: width_w = 1200, height_w = 800, width_c = 650, height_c = 650
    type(c_ptr) :: gtk_app = c_null_ptr
    type(c_ptr) :: window = c_null_ptr
    type(c_ptr) :: menu_bar = c_null_ptr
    type(c_ptr) :: canvas = c_null_ptr
    integer(c_int) :: status = 0
    character(len=16), dimension(1) :: filters = ["*.cif"], filter_names = ["CIF files"]
    logical :: has_file = .false.

    ! Current crystal
    character(:), allocatable :: crystal_name
    type(atom), allocatable :: atom_list(:)
    real(kind=8) :: crystal_a = 0.0, crystal_b = 0.0, crystal_c = 0.0

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
        if (allocated(crystal_name)) deallocate(crystal_name)
        if (allocated(atom_list)) deallocate(atom_list)

        call g_application_quit(gtk_app)
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
        call gtk_window_set_default_size(window, width_w, height_w)

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

        ! Create drawing area for the crystal display
        canvas = gtk_drawing_area_new()
        call gtk_drawing_area_set_content_width(canvas, width_c)
        call gtk_drawing_area_set_content_height(canvas, height_c)
        call gtk_drawing_area_set_draw_func(canvas, c_funloc(display_crystal), c_null_ptr, c_null_ptr)
        call gtk_window_set_child(window, canvas)

        ! Display the window
        call gtk_widget_show(window)
        call gtk_window_present(window)
    end subroutine

    ! Called when the canvas is updated, displays the crystal structure if a file is loaded
    subroutine display_crystal(widget, cairo_ctx, width, height, gdata) bind(c)
        use, intrinsic :: iso_fortran_env, only: wp=>real64
        type(c_ptr), value, intent(in) :: widget, cairo_ctx, gdata
        integer(c_int), value, intent(in) :: width, height

        ! Frame data
        real(kind=8) :: frame_x, frame_y, frame_w, frame_h
        frame_x = 10
        frame_y = 60
        frame_w = width - 20
        frame_h = height - 70
        frame_w = min(frame_w, frame_h)
        frame_h = min(frame_w, frame_h)

        ! Fill the background with a soft beige
        call cairo_set_source_rgb(cairo_ctx, 245d0/255d0, 245d0/255d0, 220d0/255d0)
        call cairo_rectangle(cairo_ctx, 0d0, 0d0, real(width, 8), real(height, 8))
        call cairo_fill(cairo_ctx)

        ! Draw crystal frame
        call cairo_set_line_width(cairo_ctx, 5.0d0)
        call cairo_set_source_rgb(cairo_ctx, 0.7d0, 0.7d0, 0.7d0)
        call cairo_rectangle(cairo_ctx, frame_x, frame_y, frame_w, frame_h)
        call cairo_stroke(cairo_ctx)

        ! Fill the background of the header with gray
        call cairo_rectangle(cairo_ctx, 0d0, 0d0, real(width, 8), 50d0)
        call cairo_fill(cairo_ctx)

        ! Fill the background of the crystal frame with white
        call cairo_set_source_rgb(cairo_ctx, 1.0d0, 1.0d0, 1.0d0)
        !call cairo_rectangle(cairo_ctx, frame_x+5, frame_y+5, frame_w-10, frame_h-10)
        call cairo_rectangle(cairo_ctx, frame_x, frame_y, frame_w, frame_h)
        call cairo_fill(cairo_ctx)

        ! Display title bar
        call cairo_set_source_rgb(cairo_ctx, 0.1d0, 0.1d0, 0.1d0)
        call cairo_set_font_size(cairo_ctx, 30d0)
        call cairo_move_to(cairo_ctx, 10d0, 35d0)

        ! If we don't have atom data yet then return
        if (has_file .eqv. .false.) then
            call cairo_show_text(cairo_ctx, "No CIF-file selected"//c_null_char)
            return
        endif

        ! Display current atom name
        call cairo_show_text(cairo_ctx, crystal_name//c_null_char)
 
        ! Draw atoms
        call draw_atom(cairo_ctx, atom_list(1), frame_x, frame_y, frame_w, frame_h)
    end subroutine

    ! Draws a single atom into the display frame
    subroutine draw_atom(cairo_ctx, curr_atom, frame_x, frame_y, frame_w, frame_h)
        type(c_ptr), value, intent(in) :: cairo_ctx
        type(atom), intent(in) :: curr_atom
        real(kind=8), intent(in) :: frame_x, frame_y, frame_w, frame_h

        real(kind=8) :: x_pos = 0, y_pos = 0, z_pos = 0, radius = 0

        ! Select color
        call cairo_set_source_rgb(cairo_ctx, 1.0d0, 0.0d0, 0.0d0)

        ! Compute position of atom relative to the frame
        x_pos = frame_x + 0.5 * frame_w
        y_pos = frame_y + 0.5 * frame_h
        radius = 10

        ! Draw atom
        call cairo_arc(cairo_ctx, x_pos, y_pos, radius, 0d0, 2 * 3.14159d0)
        call cairo_fill(cairo_ctx)
    end subroutine

    ! Called when the 'quit' button is pressed, exits the application
    subroutine quit(act, param, win) bind(c)
        type(c_ptr), value, intent(in) :: act, param, win
        call gtk_deinit_app()
    end subroutine

    ! Called when the 'Open' button is pressed, opens a .cif file
    subroutine open_file(act, param, win) bind(c)
        type(c_ptr), value, intent(in) :: act, param, win
        integer(c_int) :: ret_val
        character(len=256), dimension(:), allocatable :: selected
        character(len=256) :: file_name
        character, allocatable :: file_name2(:)

        ! Ask for file
        file_name(:) = ""
        ret_val = hl_gtk_file_chooser_show(selected, create=False, title="Select one .cif file"//c_null_char, &
            filter=filters, filter_name = filter_names, parent=window, all=TRUE)

        ! Check if a file was selected
        if (ret_val == FALSE) return
        file_name = selected(1)
        deallocate(selected)
        if (file_name(1:1) /= '/') return

        ! Open the .cif file and extract the relevant information
        has_file = .true.
        write(*, "(AA)") "[~] Opening file ", file_name
        crystal_name = cif_extract_name(file_name)
        crystal_a = cif_extract_field_real(file_name, "_cell_length_a")
        crystal_b = cif_extract_field_real(file_name, "_cell_length_b")
        crystal_c = cif_extract_field_real(file_name, "_cell_length_c")
        print *, crystal_a, crystal_b, crystal_c
        atom_list = cif_extract_atoms(file_name)
        call print_atoms(atom_list)

        ! Queue refresh
        call gtk_widget_queue_draw(canvas)
    end subroutine

end module gtk_application
