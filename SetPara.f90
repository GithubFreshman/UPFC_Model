Module  SetPara
!!!!!global variables list
implicit none
! constants常量定义
real,parameter::pi=3.14159
integer, parameter :: LineNum=100

! vars
Integer :: SystFile
Real(KIND=8),Dimension(LineNum,LineNum) :: FLOWdata         !用于存放csv文件的MTDC稳态潮流数据，必须设为方阵，且大小跟csv数据一致，否则读csv数据出?
!! powerflow parameters
integer :: N_VSC, Pole  ! N_VSC 单极VSC站个数,极数Pole，
real :: Sbase ! 功率基准值Sbase

! DC network patameters
INTEGER:: N_row, B_coln     ! N_row  -- 直流网络VSC个数!     B_coln -- 直流网络支路个数
real(kind=8), allocatable:: Ydc(:,:)
real(kind=8), allocatable:: Rb(:), Lb(:), Cb(:), Tb(:,:)
real(kind=8), allocatable:: rlm(:), xlm(:), blm0(:)
!  VSC type
type :: VSC
    integer :: No = 0, Vsctype = 0, poles =2, ctlmode = 0, ctlbus1 = 0, ctlbus2 = 0
    character(len=20) :: typestr = '0', ickt = '0'
    real :: ctlvar1 = 0, ctlvar2 = 0, Ps = 0, Qs = 0, Pdc = 0, Udc = 0, Pac1 = 0, Qac1 = 0, Pac2 = 0, Qac2 = 0
end type

public ReadFile

Contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Subroutine Initialize(Obj): initial UPFC parameters
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    Subroutine Initialize(Obj)
        implicit none
        type(VSC), Intent(Inout) :: Obj
        ! set typestr
        if (Obj%ctlmode==1) then ! control Vdc & Vac
            Obj%typestr = 'Shunt Vsc'
        else
            Obj%typestr = 'Series Vsc'
        end if
        ! set ctlmode
        
        return
    End Subroutine Initialize 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Subroutine ReadFile(): read powerflow result in csv file, set parameters to VSCs of UPFCs 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    Subroutine ReadFile()
        implicit none
        integer, parameter :: max_vscnum = 80 ! the max num of VSCs in a system, canbe changed
        integer :: cnt, ii, jj ! counter
        integer :: ierror
        character(len=80) :: filename = 'VSC_UPFCFLOW_1.csv'  ! csv filename
        character(len=2) :: str
        type(VSC),target:: myvsc(max_vscnum)

        print *, 'Test PFLOW_READ!'
        ! read csv file
        OPEN(100,FILE=filename,action ="read", status="old", iostat=ierror)!打开数据文件--(640,FILE='ZJUVSC_1.csv')--注意给ierror添加声明!
        IF ( ierror /= 0 ) THEN
            print *, "Failed to open VSC_UPFCFLOW_1.csv!"
!            WRITE(LPDEV,*) "Failed to open VSC_UPFCFLOW_1.csv!" ! using in psse
            STOP
        END IF
        
        read(100,*) FLOWdata
        close(100)
        FLOWdata = transpose(FLOWdata)      !FLOWdata转置后才跟csv的形式一样
        ! set parameters
        ! first line
        N_VSC = int(FLOWdata(1,1))
        Pole = int(FLOWdata(1,2))
        Sbase = real(FLOWdata(1,3))
        
        call ALLOCATE_ARRAY(N_VSC)
        ! other lines
        do cnt=1,N_VSC,1
            myvsc(cnt)%No = cnt
            if (cnt==1) then
                ! the first one is shunt VSC
                myvsc(cnt)%Vsctype = 1 ! shunt VSC
                myvsc(cnt)%ctlmode = 1 ! control Vdc & Vac
            else 
                ! the others are series VSC
                myvsc(cnt)%Vsctype = 2 ! series VSC
                myvsc(cnt)%ctlmode = 2 ! control Pc & Qc
            end if
            
            myvsc(cnt)%ctlbus1 = int(FLOWdata(2,cnt))
            myvsc(cnt)%ctlbus2 = int(FLOWdata(3,cnt)) !串联站BUS2为潮流控制端
            myvsc(cnt)%Udc = int(FLOWdata(4,cnt)) ! kV
            myvsc(cnt)%Pdc = int(FLOWdata(5,cnt)) ! Pdc
            myvsc(cnt)%Pac1 = int(FLOWdata(6,cnt)) ! Pac of bus1
            myvsc(cnt)%Qac1 = int(FLOWdata(7,cnt)) ! Qac of bus1
            myvsc(cnt)%Pac2 = int(FLOWdata(8,cnt)) ! Pac of bus2
            myvsc(cnt)%Qac2 = int(FLOWdata(9,cnt)) ! Qac of bus2
            
            call Initialize(myvsc(cnt)) 
!            ! 调试信息
!            write(*,"(A20,I5)") "myvsc.No:", myvsc(cnt)%No
!            write(*,"(A20,A15)") "myvsc.typestr:", myvsc(cnt)%typestr
!            write(*,"(A20,I5,I5)") "myvsc.ctlbus:", myvsc(cnt)%ctlbus1,myvsc(cnt)%ctlbus2
!           !!调试信息-结束!!!
        end do
       ! 获取直流网络拓扑，导纳矩阵，直流支路参数
        do ii=1,N_VSC
            do jj=1,N_VSC
                Ydc(ii,jj) = Dble(FLOWdata(9+ii,jj))
            end do
        end do
        
        do ii=1,B_coln
            do jj=1,N_VSC
                Tb(ii,jj) = Dble(Flowdata(9+N_VSC+ii,jj))
            end do
        end do
        
        do ii=1,B_coln
            ! 直流线路参数
            Rb(ii) = Dble(Flowdata(9+N_VSC+B_coln+1,ii))
            Lb(ii) = Dble(Flowdata(9+N_VSC+B_coln+2,ii))
            Cb(ii) = Dble(Flowdata(9+N_VSC+B_coln+3,ii))
            ! 串联线路参数
            rlm(ii) = Dble(Flowdata(9+N_VSC+B_coln+4,ii))
            xlm(ii) = Dble(Flowdata(9+N_VSC+B_coln+5,ii))
            blm0(ii) = Dble(Flowdata(9+N_VSC+B_coln+6,ii))
        end do
        
        ! 调试信息
        DO ii = 1,N_VSC
            WRITE(str,'(I2)')ii       !   将整型变量 转为 字符型
            WRITE(*,*)'Ydc['//TRIM(ADJUSTL(str))//',:]:',Ydc(ii,:)      !输出Ydc矩阵
        END DO

        WRITE(*,*)'Tb[Bran,Node]:',size(Tb,1),size(Tb,2)
        DO ii = 1,B_coln
            WRITE(str,'(I2)')ii       !   将整型变量 转为 字符型
            WRITE(*,*)'Tb['//TRIM(ADJUSTL(str))//',:]:',Tb(ii,:)  !输出Tb矩阵
        END DO
        WRITE(*,*)'Rb[Bran]-ohm:',  Rb
        WRITE(*,*)'Lb[Bran]-mH:',   Lb
        WRITE(*,*)'Cb[Bran]-uF:',   Cb
        WRITE(*,*)'rlm[Bran]-p.u.:',  rlm
        WRITE(*,*)'xlm[Bran]-p.u.:',   xlm
        WRITE(*,*)'blm0[Bran]-p.u.:',   blm0
        !!!调试信息-结束!!!
        
        print *, 'Finish PFLOW_READ!'
        
    End subroutine ReadFile
    
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !可变大小数组的内存分配子程序: ALLOCATE_ARRAY(N_node, N_row, B_coln)
    !可变大小的数组必须分配的内存大小后才能使用.
    !
    !输入：1：N_node -- 直流网络总节点数（含联络节点）
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE ALLOCATE_ARRAY(N_node)
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: N_node
        N_row = N_node
        B_coln = N_node-1
        !分配直流网络相关变量的数组大小
        ALLOCATE(Ydc(N_node,N_node))       !直流网络节点导纳矩阵
        ALLOCATE(Tb(B_coln,N_node))     !直流网络拓扑，Bran和Node在前面已获取
        ALLOCATE(Rb(B_coln))               !直流网络支路电阻,Ω
        ALLOCATE(Lb(B_coln))               !直流网络支路电感,mH
        ALLOCATE(Cb(B_coln))               !直流网络支路电容,uF
        ALLOCATE(rlm(B_coln))              !串联支路电阻 p.u.
        ALLOCATE(xlm(B_coln))              !串联支路电感 p.u.
        ALLOCATE(blm0(B_coln))             !串联支路电导 p.u.
    end subroutine ALLOCATE_ARRAY


end module SetPara
