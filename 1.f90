Module modname
  Implicit None
  !// 明确变量类型，顺序无关
  Integer , save :: a = 1 , b = 2  
End Module modname

program www_fcode_cn
  use modname !// 无需，也不能再定义 a b
  Implicit None 
  write(*,*) b , a !// 输出 2，1 正确
  a = 3
  b = 4
  !// 按变量名对应，因此倒序输出，其值也倒序
  call Sub()
End Program www_fcode_cn

Subroutine Sub()
  use modname !// 无需，也不能再定义 a b
  Implicit None
  write(*,*) a , b !// 输出 3，4 正确
End Subroutine Sub
