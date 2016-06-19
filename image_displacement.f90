program imageprocess
    implicit none
    integer,dimension(32,32):: A, B
    integer,dimension(32,32):: T
    real,dimension(1,2,1024):: V, Vd
    real,dimension(1024)::Vt, Sv
    real::Val_p, Val_0
    integer::diff
    integer:: i, j, m, n, p, s, l, n1, n2
    open(UNIT=1001,FILE='C:\Users\LicoferLocal\Documents\Visual Studio 2015\Projects\Console1\imageA.dat',STATUS='OLD',ACTION='READ')
    read(1001,'(32I3)') ((A(i, j),i=1,32),j=1,32)
    close(UNIT=1001)
    open(UNIT=1002,FILE='C:\Users\LicoferLocal\Documents\Visual Studio 2015\Projects\Console1\imageB.dat',STATUS='OLD',ACTION='READ')
    read(1002,'(32I3)') ((B(m, n),m=1,32),n=1,32)
    close(UNIT=1002)
    write(*,'(32I3)') A(1:32,1:32)
    write(*,'(32I3)') B(1:32,1:32)
    s = 1
    l = 1
    n1 = 0
    n2 = 0
! pick a pixel in pic A
    do i=1,32,1
        do j=1,32,1
! pick a pixel in pic B
            do m=1,32,1
                do n=1,32,1
! solve for the difference and record the same pixel
                    diff = A(i,j) - B(m,n)
                    if (diff == 0) then 
                        T(m,n) = 1
                    else
                        T(m,n) = 0
                    end if
                end do
            end do
!           write(*,'(32I2)') T
! record the vector
            p = 1
            do
                do m=1,32,1
                    do n=1,32,1
                        if (T(m,n) == 1) then
                            V(1,1,p) = (m-i)*1.
                            V(1,2,p) = (n-j)*1.
                        else
                            V(1,1,p) = 0.
                            V(1,2,p) = 0.
                        end if
                        p = p+1
                    end do
                end do
            if (p > 1024) exit
            end do
!           write(*,'(2F5.1)') V(1,1:2,1:1024)
! pick out the shortest vector
            do p=1,1024,1
                Vt(p) = (V(1,1,p)**2+V(1,2,p)**2)**(0.5)
            end do
!           write(*,'(32F5.1)') Vt
            Val_0 = 1024
            do p=1,1023,1
                if (Vt(p) /= 0) then
                    Val_p = Vt(p)
                end if
                if (Val_p < Val_0) then
                    Val_0 = Val_p
                end if
            end do
            do p=1,1024,1
                if (V(1,1,p) /= 0) then
                    n1 = n1+1
                else
                    n1 = n1
                end if
                if (V(1,2,p) /= 0) then
                    n2 = n2+1
                else
                    n2 = n2
                end if
            end do
!           write(*,*) 'n1=', n1, 'n2=', n2
            Vd(1,1,l) = (Sum(V(1,1,1:1024)))/n1
            Vd(1,2,l) = (Sum(V(1,2,1:1024)))/n2
            l = l+1
            Sv(s) = Val_0
            s = s+1
        end do
    end do
    open(UNIT=1003,FILE='C:\Users\LicoferLocal\Documents\Visual Studio 2015\Projects\Console1\displacement_dixtance.dat')
    write(1003,'(32F6.2)') Sv(1:1024)
    open(UNIT=1003,FILE='C:\Users\LicoferLocal\Documents\Visual Studio 2015\Projects\Console1\displacement_vector.dat')
    write(1003,100) Vd(1,1:2,1:1024)
    100 format (16('(',F4.1,',',F4.1,')'))    
end program