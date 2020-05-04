!======================Accessories Start============================================

    ! Mohammad Asif Zaman
    ! version 1.2
    !   -   2 digit date check added

	! -> Time stamp subroutine


	SUBROUTINE timestamp(str)
		CHARACTER(len=18), INTENT(out) :: str
		CHARACTER(len=2) :: dd, hr, mn, sc
		CHARACTER(len=3) :: mons(12)
		CHARACTER(len=4) :: yyyy

		INTEGER :: values(8)

		mons = ['Jan','Feb','Mar','Apr','May','Jun',&
			      'Jul','Aug','Sep','Oct','Nov','Dec']

		CALL DATE_AND_TIME(VALUES=values)


		WRITE(dd,'(i2)') values(3)
		WRITE(yyyy,'(i4)') values(1)
		WRITE(hr,'(i2)') values(5)
		WRITE(mn,'(i2)') values(6)
		WRITE(sc,'(i2)') values(7)

        ! The dd check was added in May 3, 2020.
		if (dd(1:1) == " ") then
			WRITE(dd(1:1),'(i1)') 0
		end if
		if (hr(1:1) == " ") then
			WRITE(hr(1:1),'(i1)') 0
		end if
		if (mn(1:1) == " ") then
			WRITE(mn(1:1),'(i1)') 0
		end if
		if (sc(1:1) == " ") then
			WRITE(sc(1:1),'(i1)') 0
		end if
		str = dd//mons(values(2))//yyyy//'_'//hr//'.'//mn//'.'//sc
	END SUBROUTINE timestamp
	! <- Time stamp subroutine





!======================Accessories End============================================
