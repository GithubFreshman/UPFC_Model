Module modname
  Implicit None
  !// ��ȷ�������ͣ�˳���޹�
  Integer , save :: a = 1 , b = 2  
End Module modname

program www_fcode_cn
  use modname !// ���裬Ҳ�����ٶ��� a b
  Implicit None 
  write(*,*) b , a !// ��� 2��1 ��ȷ
  a = 3
  b = 4
  !// ����������Ӧ����˵����������ֵҲ����
  call Sub()
End Program www_fcode_cn

Subroutine Sub()
  use modname !// ���裬Ҳ�����ٶ��� a b
  Implicit None
  write(*,*) a , b !// ��� 3��4 ��ȷ
End Subroutine Sub
