! Module that provides utility functions
module util
    implicit none
    public

    ! rgb color for use in gtk
    type color
        real(kind=8) :: r, g, b
    end type

contains
    ! Splits a string into tokens based on the delimiter `delim` on a trimmed input string
    function split_string(input_string, delimiter) result(tokens)
        character(*), intent(in) :: input_string
        character, intent(in) ::  delimiter
        character(256) :: temp_string
        character(64), allocatable :: tokens(:)
        integer :: i, n_tokens, token_id

        ! Compute the number of tokens
        n_tokens = 1
        do i = 1, len(input_string)
            if (input_string(i:i) == delimiter) n_tokens = n_tokens + 1
        enddo

        ! Initialize the tokens array
        allocate(tokens(n_tokens))
        tokens(:)(:) = ""
        temp_string(:) = ""

        ! Loop through the input string
        token_id = 1
        do i = 1, len(input_string)
            ! Check if we have a space
            if (input_string(i:i) == delimiter) then
                ! Add the token to the token list
                tokens(token_id) = adjustl(trim(temp_string))
                if (temp_string(1:1) /= delimiter) then
                    token_id = token_id + 1
                    temp_string(:) = ""
                endif
            else
                ! Construct the current token
                temp_string = trim(temp_string) // input_string(i:i)
            endif
        enddo

        ! Extract last element if applicable
        tokens(token_id) = adjustl(trim(temp_string))
    end function 

    ! Splits a string into tokens based on the delimiter `delim` on a trimmed input string, ignores escaped delimiters
    function split_string_escaped(input_string, delimiter) result(tokens)
        character(*), intent(in) :: input_string
        character, intent(in) ::  delimiter
        character(256) :: temp_string
        character(64), allocatable :: tokens(:)
        integer :: i, n_tokens, token_id
        logical :: escaped = .false., was_delim = .false.

        ! Compute the number of tokens
        n_tokens = 1
        do i = 1, len(input_string)
            if (input_string(i:i) == delimiter) n_tokens = n_tokens + 1
        enddo

        ! Initialize the tokens array
        allocate(tokens(n_tokens))
        tokens(:)(:) = ""
        temp_string(:) = ""

        ! Loop through the input string
        token_id = 1
        do i = 1, len(input_string)
            ! Check if we have an escape char
            if (input_string(i:i) == "'" .or. input_string(i:i) == '"') then
                escaped = .not. escaped
                cycle
            endif

            ! Check if we have a space
            if (input_string(i:i) == delimiter .and. .not. escaped) then
                ! Add the token to the token list
                tokens(token_id) = adjustl(trim(temp_string))
                if (temp_string(1:1) /= delimiter) then
                    token_id = token_id + 1
                    temp_string(:) = ""
                endif
            else
                ! Construct the current token
                if (was_delim) then
                    temp_string = trim(temp_string) // " " // input_string(i:i)
                    was_delim = .false.
                else
                    temp_string = trim(temp_string) // input_string(i:i)
                endif

                ! Check if we added an escaped delimiter
                if (input_string(i:i) == delimiter) was_delim = .true.
            endif
        enddo

        ! Extract last element if applicable
        tokens(token_id) = adjustl(trim(temp_string))
    end function

	! Removes leading whitespaces from a string
    subroutine shift_string_left(string)
        character(len=*), intent(inout) :: string
        integer :: i

		! Iterate over characters and find where the actual string starts
		do i = 1, len(string)
			if (string(i:i) /= " ") then
				string = string(i:len(string))
				return
			end if
		end do
    end subroutine
end module util
