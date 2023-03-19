! Module that provides utility functions
module util
    implicit none
    public

contains
    ! Splits a string into tokens based on the delimiter ' ' on a trimmed input string
    function split_string(input_string) result(tokens)
        character(*), intent(in) :: input_string
        character(256) :: temp_string = ""
        character(32), allocatable :: tokens(:)
        integer :: i, n_tokens, token_id

        ! Compute the number of tokens
        n_tokens = 1
        do i = 1, len(input_string)
            if (input_string(i:i) == ' ') n_tokens = n_tokens + 1
        enddo

        ! Initialize the tokens array
        allocate(tokens(n_tokens))
        tokens(:) = ""
        temp_string(:) = ""

        ! Loop through the input string
        token_id = 1
        do i = 1, len(input_string)-1
            ! Check if we have a space
            if (input_string(i:i) == " ") then
                ! Add the token to the token list
                tokens(token_id) = adjustl(trim(temp_string))
                token_id = token_id + 1
                temp_string(:) = ""
            else
                ! Construct the current token
                temp_string = trim(temp_string) // input_string(i:i)
            endif
        enddo
    end function 
end module util
