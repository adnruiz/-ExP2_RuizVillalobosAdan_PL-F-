import scala.util.Random

class Alumno(private var numControl:String, private var nombre:String,
             private var primerAp:String, private var segundoAp:String, private var fechaNacimiento:String,
             private var semestre: Byte, private var carrera:String,
             private var calificacionesParcialesxMateria: List[List[Int]]){

  //Getters
  def getNumControl():String={
    numControl
  }
  def getNombre():String={
    nombre
  }
  def getPrimerAp():String={
    primerAp
  }
  def getSegundoAp():String={
    segundoAp
  }
  def getFechaNacimiento():String={
    fechaNacimiento
  }
  def getSemestre():Byte={
    semestre
  }
  def getCarrera():String={
    carrera
  }
  def getCalificacionesParcialesxMateria():List[List[Int]]={
    calificacionesParcialesxMateria
  }

  //Setters
  def setNumControl(numC:String){
    numControl = numC
  }
  def setNombre(nom:String){
    nombre = nom
  }
  def setPrimerAp(pa:String){
    primerAp = pa
  }
  def setSegundoAp(sa:String){
    segundoAp=sa
  }
  def setFechaNacimiento(fecha:String){
    fechaNacimiento=fecha
  }
  def setSemestre(sem:Byte){
    semestre=sem
  }
  def setCarrera(carr:String){
    carrera=carr
  }
  def setCalificacionesParcialesxMateria(calif:List[List[Int]]): Unit ={
    calificacionesParcialesxMateria = calif
  }

  def obtenerPromedio():Double={
    var materia1, materia2, materia3, materia4, materia5, materia6, promedio = 0
    for(i<-0 until getCalificacionesParcialesxMateria().length){
      for(j<-0 until getCalificacionesParcialesxMateria()(i).length){
        if(i==0){
          materia1 = materia1 + getCalificacionesParcialesxMateria()(i)(j)
        }else if(i==1){
          materia2 = materia2 + getCalificacionesParcialesxMateria()(i)(j)
        }else if(i==2){
          materia3 = materia3 + getCalificacionesParcialesxMateria()(i)(j)
        }else if(i==3){
          materia4 = materia4 + getCalificacionesParcialesxMateria()(i)(j)
        }else if(i==4){
          materia5 = materia5 + getCalificacionesParcialesxMateria()(i)(j)
        }else if(i==5){
          materia6 = materia6 + getCalificacionesParcialesxMateria()(i)(j)
        }
      }
    }
    promedio = ((materia1/4)+(materia2/4)+(materia3/4)+(materia4/4)+materia5/4+(materia6/4))/6
    promedio
  }

  def obtenerRFC():String={
    var RFC = ""
    var fechaN = getFechaNacimiento()
    var cont = 0

    for(i <- 0 until getPrimerAp().length()){
      if (i==0){
        RFC = RFC + getPrimerAp().substring(0,2)
      }//cierra if para primeros caracteres del apellido paterno
      while (cont == 1){
        if(getPrimerAp().charAt(i).equals('a')||getPrimerAp().charAt(i).equals('e')||getPrimerAp().charAt(i).equals('i')||getPrimerAp().charAt(i).equals('o')||getPrimerAp().charAt(i).equals('u')){
          cont += 1
          RFC =RFC + getPrimerAp().charAt(i).toString
        }//cierra el if para vocales
      }//cierra el while
    }//cierra el for para recorrer el apellido paterno

    RFC = RFC + getSegundoAp().charAt(0).toString
    RFC = RFC + getNombre().charAt(0).toString

    for(j<-0 until getFechaNacimiento().length()){
      if(j==2 || j==3 || j==5 || j==6 || j==8 || j==9){
        RFC = RFC + getFechaNacimiento().charAt(j).toString
      }
    }

    RFC.toUpperCase()
  }//termina funcion para obtener RFC

  def obtenerCalificacionesAprobatorias():Unit={
    println("Calificaciones aprobatorias")
    for(i <- 0 until getCalificacionesParcialesxMateria().length){
      for(j<- 0 until getCalificacionesParcialesxMateria()(i).length){
        if(getCalificacionesParcialesxMateria()(i)(j)>69){
          println("Parcial: " + (i+1) + " Calificacion: " + getCalificacionesParcialesxMateria()(i)(j))
        }
      }
    }
  }

}// termina la clase alumno

object Prueba {
  def materiasReprobadas(listaDeListas: List[List[Int]]): Int={
    var contReprobadas=0
    var materia1, materia2, materia3, materia4, materia5, materia6 = 0

    for(i <- 0 until listaDeListas.length){
      for(j <- 0 until listaDeListas(i).length){
        if(i==0){
          if(listaDeListas(i)(j) < 70){
            materia1 = materia1 + 1
          }
        }else if(i==1){
          if(listaDeListas(i)(j) < 70){
            materia2 = materia2 + 1
          }
        }else if(i==1){
          if(listaDeListas(i)(j) < 70){
            materia3 = materia3 + 1
          }
        }else if(i==1){
          if(listaDeListas(i)(j) < 70){
            materia4 = materia4 + 1
          }
        }else if(i==1){
          if(listaDeListas(i)(j) < 70){
            materia5 = materia5 + 1
          }
        }else if(i==1){
          if(listaDeListas(i)(j) < 70){
            materia6 = materia6 + 1
          }
        }
      }
    }

    var arregloDeMaterias = Array(materia1, materia2, materia3, materia4, materia5, materia6)
    var arregloReversa = arregloDeMaterias.sorted(Ordering.Int.reverse)

    for (j <- 0 until arregloDeMaterias.length){
      if(arregloReversa(0).equals(arregloDeMaterias(j))){
        contReprobadas = j+1
      }
    }
    contReprobadas
  }

  def materiasMasRerpobadas(listaDeListas: List[List[Int]]): Int={
    var contMasReprobadas=0
    var materia1, materia2, materia3, materia4, materia5, materia6 = 0

    for(i <- 0 until listaDeListas.length){
      for(j <- 0 until listaDeListas(i).length){
        if(i==0){
          if(listaDeListas(i)(j) < 70){
            materia1 = materia1 + 1
          }
        }else if(i==1){
          if(listaDeListas(i)(j) < 70){
            materia2 = materia2 + 1
          }
        }else if(i==1){
          if(listaDeListas(i)(j) < 70){
            materia3 = materia3 + 1
          }
        }else if(i==1){
          if(listaDeListas(i)(j) < 70){
            materia4 = materia4 + 1
          }
        }else if(i==1){
          if(listaDeListas(i)(j) < 70){
            materia5 = materia5 + 1
          }
        }else if(i==1){
          if(listaDeListas(i)(j) < 70){
            materia6 = materia6 + 1
          }
        }
      }
    }

    var arregloDeMaterias = Array(materia1, materia2, materia3, materia4, materia5, materia6)


    for (j <- 0 until arregloDeMaterias.length){
      if(materia1 > materia2 || materia1 > materia3 || materia1 > materia4 || materia1 > materia5 || materia1 > materia6){
        contMasReprobadas = j+1
      }else if(materia2 > materia1 || materia2 > materia3 || materia2 > materia4 || materia2 > materia5 || materia2 > materia6){
        contMasReprobadas = j + 1
      }else if(materia3 > materia1 || materia3 > materia2 || materia3 > materia4 || materia3 > materia5 || materia3 > materia6){
        contMasReprobadas = j + 1
      }else if(materia4 > materia1 || materia4 > materia2 || materia4 > materia3 || materia4 > materia5 || materia4 > materia6){
        contMasReprobadas = j + 1
      }else if(materia5 > materia1 || materia5 > materia2 || materia5 > materia4 || materia5 > materia3 || materia5 > materia6){
        contMasReprobadas = j + 1
      }else if(materia6 > materia1 || materia6 > materia2 || materia6 > materia4 || materia6 > materia5 || materia6 > materia5){
        contMasReprobadas = j + 1
      }

    }
    contMasReprobadas
  }

  def main(args: Array[String]): Unit = {
    val listaCalificaciones1:List[List[Int]] =
      List.fill(6)(List.fill(4)(Random.between(0, 100)))
    val listaCalificaciones2:List[List[Int]] =
      List.fill(6)(List.fill(4)(Random.between(0, 100)))
    val listaCalificaciones3:List[List[Int]] =
      List.fill(6)(List.fill(4)(Random.between(0, 100)))
    val listaCalificaciones4:List[List[Int]] =
      List.fill(6)(List.fill(4)(Random.between(0, 100)))
    val listaCalificaciones5:List[List[Int]] =
      List.fill(6)(List.fill(4)(Random.between(0, 100)))
    val listaCalificaciones6:List[List[Int]] =
      List.fill(6)(List.fill(4)(Random.between(0, 100)))

    var alumno1 = new Alumno("1","Adan", "Ruiz", "Villalobos", "1998/06/28", 8, "ISC", listaCalificaciones1)
    var alumno2 = new Alumno("2","Katherine", "Diaz", "Gonzales", "1999/10/22", 8, "ARQ", listaCalificaciones2)
    var alumno3 = new Alumno("3","Ricardo", "Alcala", "Ruiz", "1998/04/18", 8, "IM", listaCalificaciones3)
    var alumno4 = new Alumno("4","David", "Alcala", "Ruiz", "1998/07/01", 8, "ISC", listaCalificaciones4)
    var alumno5 = new Alumno("5","Thiago", "Cabral", "Ruiz", "1998/01/10", 8, "ISC", listaCalificaciones5)

    println("Promedio General del Grupo " + (alumno1.obtenerPromedio() + alumno2.obtenerPromedio() + alumno3.obtenerPromedio() + alumno4.obtenerPromedio() + alumno5.obtenerPromedio())/5)
    println("Alumno 1: ")
    println("El promedio es: " +alumno1.obtenerPromedio())
    println("El RFC es: "+alumno1.obtenerRFC())
    alumno1.obtenerCalificacionesAprobatorias()
    println("La materia mas reprobada es: " + materiasReprobadas(alumno1.getCalificacionesParcialesxMateria()))
    println("El parcial mas reprobado: " + materiasMasRerpobadas(alumno1.getCalificacionesParcialesxMateria()))

    println("Alumno 2: ")
    println("El promedio es: " +alumno2.obtenerPromedio())
    println("El RFC es: "+alumno2.obtenerRFC())
    alumno3.obtenerCalificacionesAprobatorias()
    println("La materia mas reprobada es: " + materiasReprobadas(alumno2.getCalificacionesParcialesxMateria()))
    println("El parcial mas reprobado: " + materiasMasRerpobadas(alumno2.getCalificacionesParcialesxMateria()))

    println("Alumno 3: ")
    println("El promedio es: " +alumno3.obtenerPromedio())
    println("El RFC es: "+alumno3.obtenerRFC())
    alumno3.obtenerCalificacionesAprobatorias()
    println("La materia mas reprobada es: " + materiasReprobadas(alumno3.getCalificacionesParcialesxMateria()))
    println("El parcial mas reprobado: " + materiasMasRerpobadas(alumno3.getCalificacionesParcialesxMateria()))

    println("Alumno 4: ")
    println("El promedio es: " +alumno4.obtenerPromedio())
    println("El RFC es: "+alumno4.obtenerRFC())
    alumno4.obtenerCalificacionesAprobatorias()
    println("La materia mas reprobada es: " + materiasReprobadas(alumno4.getCalificacionesParcialesxMateria()))
    println("El parcial mas reprobado: " + materiasMasRerpobadas(alumno4.getCalificacionesParcialesxMateria()))

    println("Alumno 5: ")
    println("El promedio es: " +alumno5.obtenerPromedio())
    println("El RFC es: "+alumno5.obtenerRFC())
    alumno5.obtenerCalificacionesAprobatorias()
    println("La materia mas reprobada es: " + materiasReprobadas(alumno5.getCalificacionesParcialesxMateria()))
    println("El parcial mas reprobado: " + materiasMasRerpobadas(alumno5.getCalificacionesParcialesxMateria()))
  }

}
