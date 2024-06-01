! Hay 20 renglones en el archivo de entrada, cada renglon tiene 6 elementos
! Se deben ordenarlos datos de cada renglon, los 6 valores mas parecidos van apareciendo a la esquina de la grafica
! La y son los numeros, la x es cada renglon

Program Proyecto_1
    implicit none
! Declaramos variables, una matriz con 20 columnas y 7 filas, cada columna es un renglón del archivo, declaramos el array de varianzas
    real, dimension(7,20) :: matrix !Matriz de datos
    real, dimension(20) :: v_array !array de varianza 
    real :: prom, var, sum, sumSQ
    integer :: i, n, j


!Leemos los datos de la matriz, dejamos el primer valor de cada columna vacío, empezamos desde la segunda fila
    open(1, file='T_F.txt')

    Do j=1,20

        Read(1,*) (matrix(i,j), i=2,7)        

    end do

    close(1)

! Llamamos a la subrutina para crear el arreglo de varianzas escaladas y llenar la primera fila de cada columna

    call variance(matrix, v_array, Sum, SumSQ, prom, var)

! Llamamos a la subrutina que ordenará el array de varianzas 

    call Shell_Sort(v_array)

    write(*,*) 'Varianzas ordenadas'
    Do n=1,20 

        write(*,*) v_array(n)

    end do 
! Hacemos un ciclo que recorra cada columna, con un if que compare la primer fila de cada columna con el primer valor del
! Arreglo de varianzas escaladas ordenado. Si hay coincidencia, escribe todas las columnas de esa fila (exceptuando la primera) en un documento de salida
    
    open(2, file='TF_output.txt')

    Do j=1,20
        Do n=1,20

            If (v_array(j)==matrix(1,n)) then
                
                Do i=2,7
                Write(2,*) j, matrix(i,n)
                end do
                exit
            Else
                write(*,*) 'No coincidencia en el intento', n, 'De la posicion', j 
            end if

        end do
    end do

    close(2)


! El archivo de salida es como el de entrada, pero ahora el primer renglon es el de menor varianza y cada renglon tiene más varianza que el anterior
! Graficamos el archvio de salida generando un gpl
    call graph()

      call system('start gnuplot -p grafica.gpl')

      print*, 'espere a que se genere la grafica y de enter para abrirla'
      read(*,*)

      call system('Proyecto1.png')

end program

!Crear subrutina que calcule la varianza de cada columna y la escriba en la primer fila de cada columna, además de escribirla en un arreglo de varianzas
Subroutine variance(matrix, v_array, Sum, SumSQ, prom, var)

    implicit none 

    real, dimension(7,20) :: matrix !Matriz de datos
    real, dimension(20) :: v_array !array de varianza 
    real :: prom, var, sum, sumSQ
    integer :: i, j

    Do j=1,20

        sum = 0 
        sumSQ = 0
        Do i=2,7
            sum = sum + matrix(i,j)
            sumSQ = sumSQ + matrix(i,j) * matrix(i,j)
        end do
        prom = sum/6
        var = (sumSQ - sum*sum/6)*100
        matrix(1,j) = var 
        v_array(j) = var

    end do
End subroutine
!Crear subrutina que ordene el arreglo de varianzas de forma ascendente 
SUBROUTINE Shell_Sort(a)

    IMPLICIT NONE
    INTEGER :: i, j, increment
    REAL :: temp
    REAL, INTENT(in out) :: a(20)
      
    increment = SIZE(a) / 2
    DO WHILE (increment > 0)
        DO i = increment+1, SIZE(a)
           j = i
           temp = a(i)
           DO WHILE (j >= increment+1 .AND. a(j-increment) > temp)
              a(j) = a(j-increment)
              j = j - increment
           END DO
           a(j) = temp
        END DO
        IF (increment == 2) THEN
           increment = 1
        ELSE
           increment = increment * 5 / 11
        END IF      
    END DO 
  END SUBROUTINE Shell_Sort

! Subrutina para graficar 

subroutine Graph()
    open (439, file='grafica.gpl')
    write(439,*) "set terminal pngcairo enhanced font 'Verdana,12'"
    write(439,*) "set output 'Proyecto1.png'"
    write(439,*) "set style line 1 lt 1 lw 2 pt 7 ps 1.5 lc rgb 'blue'"
    write(439,*) "set title 'Proyecto1'"
    write(439,*) "set xlabel 'Número de vector'"
    write(439,*) "set ylabel 'Valores de vector'"
    write(439,*) "set yrange [2.6:3.6]"
    write(439,*) "set xrange [0:21]"
    write(439,*) "set border linewidth 1.5"
    write(439,*) 'set xzeroaxis linetype 1 linewidth 2.5'
    write(439,*) "set yzeroaxis linetype 1 linewidth 2.5"
    write(439,*) "set grid"
    write(439,*) "plot 'TF_output.txt' with points ps 2 pt 7"
    close(439)
    RETURN
end subroutine Graph
  