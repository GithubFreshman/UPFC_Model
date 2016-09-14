Module  SetPara
!!!!!global variables list
implicit none
! constants��������
real,parameter::pi=3.14159
integer, parameter :: LineNum=100

! vars
Integer :: SystFile
Real(KIND=8),Dimension(LineNum,LineNum) :: FLOWdata         !���ڴ��csv�ļ���MTDC��̬�������ݣ�������Ϊ�����Ҵ�С��csv����һ�£������csv���ݳ�?
!! powerflow parameters
integer :: N_VSC, Pole  ! N_VSC ����VSCվ����,����Pole��
real :: Sbase ! ���ʻ�׼ֵSbase

! DC network patameters
INTEGER:: N_row, B_coln     ! N_row  -- ֱ������VSC����!     B_coln -- ֱ������֧·����
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
        OPEN(100,FILE=filename,action ="read", status="old", iostat=ierror)!�������ļ�--(640,FILE='ZJUVSC_1.csv')--ע���ierror�������!
        IF ( ierror /= 0 ) THEN
            print *, "Failed to open VSC_UPFCFLOW_1.csv!"
!            WRITE(LPDEV,*) "Failed to open VSC_UPFCFLOW_1.csv!" ! using in psse
            STOP
        END IF
        
        read(100,*) FLOWdata
        close(100)
        FLOWdata = transpose(FLOWdata)      !FLOWdataת�ú�Ÿ�csv����ʽһ��
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
            myvsc(cnt)%ctlbus2 = int(FLOWdata(3,cnt)) !����վBUS2Ϊ�������ƶ�
            myvsc(cnt)%Udc = int(FLOWdata(4,cnt)) ! kV
            myvsc(cnt)%Pdc = int(FLOWdata(5,cnt)) ! Pdc
            myvsc(cnt)%Pac1 = int(FLOWdata(6,cnt)) ! Pac of bus1
            myvsc(cnt)%Qac1 = int(FLOWdata(7,cnt)) ! Qac of bus1
            myvsc(cnt)%Pac2 = int(FLOWdata(8,cnt)) ! Pac of bus2
            myvsc(cnt)%Qac2 = int(FLOWdata(9,cnt)) ! Qac of bus2
            
            call Initialize(myvsc(cnt)) 
!            ! ������Ϣ
!            write(*,"(A20,I5)") "myvsc.No:", myvsc(cnt)%No
!            write(*,"(A20,A15)") "myvsc.typestr:", myvsc(cnt)%typestr
!            write(*,"(A20,I5,I5)") "myvsc.ctlbus:", myvsc(cnt)%ctlbus1,myvsc(cnt)%ctlbus2
!           !!������Ϣ-����!!!
        end do
       ! ��ȡֱ���������ˣ����ɾ���ֱ��֧·����
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
            ! ֱ����·����
            Rb(ii) = Dble(Flowdata(9+N_VSC+B_coln+1,ii))
            Lb(ii) = Dble(Flowdata(9+N_VSC+B_coln+2,ii))
            Cb(ii) = Dble(Flowdata(9+N_VSC+B_coln+3,ii))
            ! ������·����
            rlm(ii) = Dble(Flowdata(9+N_VSC+B_coln+4,ii))
            xlm(ii) = Dble(Flowdata(9+N_VSC+B_coln+5,ii))
            blm0(ii) = Dble(Flowdata(9+N_VSC+B_coln+6,ii))
        end do
        
        ! ������Ϣ
        DO ii = 1,N_VSC
            WRITE(str,'(I2)')ii       !   �����ͱ��� תΪ �ַ���
            WRITE(*,*)'Ydc['//TRIM(ADJUSTL(str))//',:]:',Ydc(ii,:)      !���Ydc����
        END DO

        WRITE(*,*)'Tb[Bran,Node]:',size(Tb,1),size(Tb,2)
        DO ii = 1,B_coln
            WRITE(str,'(I2)')ii       !   �����ͱ��� תΪ �ַ���
            WRITE(*,*)'Tb['//TRIM(ADJUSTL(str))//',:]:',Tb(ii,:)  !���Tb����
        END DO
        WRITE(*,*)'Rb[Bran]-ohm:',  Rb
        WRITE(*,*)'Lb[Bran]-mH:',   Lb
        WRITE(*,*)'Cb[Bran]-uF:',   Cb
        WRITE(*,*)'rlm[Bran]-p.u.:',  rlm
        WRITE(*,*)'xlm[Bran]-p.u.:',   xlm
        WRITE(*,*)'blm0[Bran]-p.u.:',   blm0
        !!!������Ϣ-����!!!
        
        print *, 'Finish PFLOW_READ!'
        
    End subroutine ReadFile
    
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !�ɱ��С������ڴ�����ӳ���: ALLOCATE_ARRAY(N_node, N_row, B_coln)
    !�ɱ��С��������������ڴ��С�����ʹ��.
    !
    !���룺1��N_node -- ֱ�������ܽڵ�����������ڵ㣩
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE ALLOCATE_ARRAY(N_node)
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: N_node
        N_row = N_node
        B_coln = N_node-1
        !����ֱ��������ر����������С
        ALLOCATE(Ydc(N_node,N_node))       !ֱ������ڵ㵼�ɾ���
        ALLOCATE(Tb(B_coln,N_node))     !ֱ���������ˣ�Bran��Node��ǰ���ѻ�ȡ
        ALLOCATE(Rb(B_coln))               !ֱ������֧·����,��
        ALLOCATE(Lb(B_coln))               !ֱ������֧·���,mH
        ALLOCATE(Cb(B_coln))               !ֱ������֧·����,uF
        ALLOCATE(rlm(B_coln))              !����֧·���� p.u.
        ALLOCATE(xlm(B_coln))              !����֧·��� p.u.
        ALLOCATE(blm0(B_coln))             !����֧·�絼 p.u.
    end subroutine ALLOCATE_ARRAY


end module SetPara
