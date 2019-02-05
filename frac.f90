! frac.f90
!    Fortran 2008 (or something like that) source code
!    requires: gfortran (or similar), unicode support in terminal
!    compile command: gfortran frac.f90 -std=f2008 -o frac
!    command to run : ./frac

PROGRAM fractal

! Program generates the Mandlebrot fractal and animates a gentle zoom.
! The animation is achieved by sending a new screen of text to the terminal,
! allowing the old screen to scroll up, and hopefully pausing to recompute
! the new fractal long enough to create an animation effect.
! The drawing is done with unicode block characters, eg. ▚.  Fortran
! does not seem to support unicode, so I experimentally found a method that
! appears to work, at least with gfortran.

IMPLICIT NONE

! Set these as desired.

INTEGER, PARAMETER    :: terminal_width=190, &
                         terminal_height=60, & 
                         iterations=20,      &
                         delay_loop_count=0

COMPLEX               :: z, c
LOGICAL               :: escape, ul, ur, ll, lr
INTEGER               :: x, y, i, xb, yb, frame
REAL                  :: zoom
                       ! The screen in memory, in a way,
                       ! to generate the fractal before
                       ! writing it to screen.
CHARACTER (LEN=50000) :: text
                       ! This will hold the unicode glyph.
CHARACTER (LEN=3)     :: unicode

DO frame=150, 1000

   text = ""

   zoom = 150.0/frame
   
   ! Loop through the terminal character positions
   DO y=0, terminal_height
      DO x=0, terminal_width
         ! With the unicode characters, we can double
         ! the resolution.  ul referes to whether the 
         ! upper left part of the block should be pained
         ! and the others are likewise.
         ul = .FALSE.
         ur = .FALSE.
         ll = .FALSE.
         lr = .FALSE.
         DO xb=0, 1
            DO yb = 0, 1
              ! standard z and c of the Mandlebrot definition
              z = cmplx(0, 0, kind=8)
              ! The zoom factor is a bit hacked together, but it works.
              c = cmplx ( 0-zoom*real(x*2+xb)/120 , ((zoom*real(y*2+yb)-60)/60), kind=8)
              
              ! Did the z value become (pretty) unbounded?
              escape = .FALSE.
              i=0
              DO WHILE (i < iterations)
                 z = z**2 + c
                 IF (ABS(AIMAG(z)) > 2 ) THEN
                    escape = .TRUE.
                    i = iterations
                 END IF
                 IF (ABS(REAL(z)) > 2 ) THEN
                    escape = .TRUE.
                    i = iterations
                 END IF
                 i = i + 1
              END DO

              ! Determine which parts of the charater need painting
              IF (escape .EQV. .TRUE.) THEN
                 IF ((xb == 0) .AND. (yb == 0)) THEN
                    ul = .TRUE.
                 END IF
                 IF ((xb == 1) .AND. (yb == 0)) THEN
                    ur = .TRUE.
                 END IF
                 IF ((xb == 0) .AND. (yb == 1)) THEN
                    ll = .TRUE.
                 END IF
                 IF ((xb == 1) .AND. (yb == 1)) THEN
                    lr = .TRUE.
                 END IF
              END IF
            END DO
         END DO
      
         ! There's probably a better way to do this, but decide the character to print based on which
         ! sections must be painted and set unicode to that character.

        IF ((ul .EQV. .TRUE.) .AND. (ur .EQV. .FALSE.) .AND. (lr .EQV. .FALSE.) .AND. (ll .EQV. .FALSE.)) THEN
          WRITE (unicode, "(A3)" ) "▘"
        END IF
        IF ((ul .EQV. .FALSE.) .AND. (ur .EQV. .TRUE.) .AND. (lr .EQV. .FALSE.) .AND. (ll .EQV. .FALSE.)) THEN
           WRITE (unicode, "(A3)" ) "▝"
        END IF

        IF ((ul .EQV. .TRUE.) .AND. (ur .EQV. .TRUE.) .AND. (lr .EQV. .FALSE.) .AND. (ll .EQV. .FALSE.)) THEN
           WRITE (unicode, "(A3)" ) "▀"
        END IF

        IF ((ul .EQV. .FALSE.) .AND. (ur .EQV. .FALSE.) .AND. (lr .EQV. .TRUE.) .AND. (ll .EQV. .FALSE.)) THEN
           WRITE (unicode, "(A3)" ) "▗"
        END IF

        IF ((ul .EQV. .TRUE.) .AND. (ur .EQV. .FALSE.) .AND. (lr .EQV. .TRUE.) .AND. (ll .EQV. .FALSE.)) THEN
           WRITE (unicode, "(A3)" ) "▚"
        END IF

        IF ((ul .EQV. .FALSE.) .AND. (ur .EQV. .TRUE.) .AND. (lr .EQV. .TRUE.) .AND. (ll .EQV. .FALSE.)) THEN
           WRITE (unicode, "(A3)" ) "▐"
        END IF

        IF ((ul .EQV. .TRUE.) .AND. (ur .EQV. .TRUE.) .AND. (lr .EQV. .TRUE.) .AND. (ll .EQV. .FALSE.)) THEN
           WRITE (unicode, "(A3)" ) "▜"
        END IF

        IF ((ul .EQV. .FALSE.) .AND. (ur .EQV. .FALSE.) .AND. (lr .EQV. .FALSE.) .AND. (ll .EQV. .TRUE.)) THEN
           WRITE (unicode, "(A3)" ) "▖"
        END IF

        IF ((ul .EQV. .TRUE.) .AND. (ur .EQV. .FALSE.) .AND. (lr .EQV. .FALSE.) .AND. (ll .EQV. .TRUE.)) THEN
           WRITE (unicode, "(A3)" ) "▌"
        END IF
        IF ((ul .EQV. .FALSE.) .AND. (ur .EQV. .TRUE.) .AND. (lr .EQV. .FALSE.) .AND. (ll .EQV. .TRUE.)) THEN
           WRITE (unicode, "(A3)" ) "▞"
        END IF

        IF ((ul .EQV. .TRUE.) .AND. (ur .EQV. .TRUE.) .AND. (lr .EQV. .FALSE.) .AND. (ll .EQV. .TRUE.)) THEN
           WRITE (unicode, "(A3)" ) "▛"
        END IF

        IF ((ul .EQV. .FALSE.) .AND. (ur .EQV. .FALSE.) .AND. (lr .EQV. .TRUE.) .AND. (ll .EQV. .TRUE.)) THEN
           WRITE (unicode, "(A3)" ) "▄"
        END IF

        IF ((ul .EQV. .TRUE.) .AND. (ur .EQV. .FALSE.) .AND. (lr .EQV. .TRUE.) .AND. (ll .EQV. .TRUE.)) THEN
           WRITE (unicode, "(A3)" ) "▙"
        END IF

        IF ((ul .EQV. .FALSE.) .AND. (ur .EQV. .TRUE.) .AND. (lr .EQV. .TRUE.) .AND. (ll .EQV. .TRUE.)) THEN
           WRITE (unicode, "(A3)" ) "▟"
        END IF

        IF ((ul .EQV. .TRUE.) .AND. (ur .EQV. .TRUE.) .AND. (lr .EQV. .TRUE.) .AND. (ll .EQV. .TRUE.)) THEN
           WRITE (unicode, "(A3)" ) "█"
        END IF
         
         ! Space is a special case, otherwise add the unicode character to the variable text.
         IF ((ul .EQV. .FALSE.) .AND. (ur .EQV. .FALSE.) .AND. (lr .EQV. .FALSE.) .AND. (ll .EQV. .FALSE.)) THEN

           text = TRIM(text) // "-"
         ELSE
                      text = TRIM(text) // unicode
        END IF

      END DO
    text = TRIM(text) // CHAR(10) ! new line character
   END DO

   ! If I used space, it would have trimmed it off, so I used '-'.  Now replace it with space.
   i = 1
   DO WHILE ( i > 0  )
      i = scan(text, "-")
      text(i:i) = " "

   END DO

   WRITE(*,"(A)",  advance="no") trim(text)
   
   ! Delay in case the animation is too fast.
   DO i=1, delay_loop_count
   END DO
END DO

END PROGRAM fractal
