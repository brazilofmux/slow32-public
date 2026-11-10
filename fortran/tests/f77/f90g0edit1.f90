! G0 with no .d: the number as list-directed output has it, no blanks.
! (Its exponent form is left out: gfortran 14 and 16 print it differently.)
program p
  write(*,'(G0,"|",G0,"|",G0,"|",G0)') 1.5, 2.5d0, 7, 'ab'
  write(*,'(1PG12.0,"|",G12.4,"|")') 1.5, 2.5d0
  write(*,'(G0,1X,G0,1X,G0)') 0.0, -0.25, 123456.75d0
end
