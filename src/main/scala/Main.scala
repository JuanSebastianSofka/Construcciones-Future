import recursos.RecursosMateriales
import solicitud.{OrdenConstruccion, SolicitudConstruccion}
import tiposconstrucciones.{CanchaFutbol, Casa, Edificio, Gimnasio, Lago}
import validaciones.Validaciones

import scala.io.StdIn.readLine
import org.joda.time.DateTime

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Main {
  def main(args: Array[String]): Unit = {
    val recursosIniciales: RecursosMateriales = RecursosMateriales(cemento = 1000, grava = 1000, arena = 1000, madera = 1000, adobe = 1000)
    var recursosActuales = recursosIniciales

    var ordenesPendientes: List[OrdenConstruccion] = List()
    var ordenesEnProgreso: List[OrdenConstruccion] = List()
    var ordenesFinalizadas: List[OrdenConstruccion] = List()

    var listaSolicitudes: List[SolicitudConstruccion] = List()
    var ordendesConstruccion: List[OrdenConstruccion] = List()

    var diaSimulado = DateTime.now() //simula el paso de los días para verificar las solicitudes pendientes, en progreso y finalizadas

    var opcionProceso = ""

    while (opcionProceso != "0") {
      println(
        """Ingrese el proceso que desea realizar
          |0. Cancelar
          |1. Ingresar nueva solicitud
          |2. Consultar fecha de terminacion del proyecto de la ciudadela
          |3. Informe solicitudes pendientes
          |4. Informe solicitudes en progreso
          |5. Informe solicitudes finalizadas
          |""".stripMargin)

      opcionProceso = readLine()

      opcionProceso match {
        case "0" => opcionProceso = "0" //0. Cancelar

        case "1" => //1. Ingresar nueva solicitud
          var opcionSolicitud = ""

          while (opcionSolicitud != "0") {
            println(
              """
                |¿Qué tipo de solicitud desea hacer?
                |0. Salir
                |1. Casa
                |2. Lago
                |3. Cancha de Fútbol
                |4. Edificio
                |5. Gimnasio
                |""".stripMargin)

            opcionSolicitud = readLine()

            opcionSolicitud match {
              case "0" => opcionSolicitud = "0" //0. Salir

              case "1" => //1. Casa
                val casaFuturo = Future {
                  Validaciones.manejoOpcionesSolicitudes(listaSolicitudes, ordendesConstruccion, recursosActuales, Casa)
                }
                diaSimulado = diaSimulado.plusHours(24)

                listaSolicitudes = Await.result(casaFuturo, Duration.Inf)._1
                ordendesConstruccion = Await.result(casaFuturo, Duration.Inf)._2
                recursosActuales = Await.result(casaFuturo, Duration.Inf)._3
                opcionSolicitud = Await.result(casaFuturo, Duration.Inf)._4

              case "2" => //2. Lago
                val lagoFuturo = Future {
                  Validaciones.manejoOpcionesSolicitudes(listaSolicitudes, ordendesConstruccion, recursosActuales, Lago)
                }
                diaSimulado = diaSimulado.plusHours(24)

                listaSolicitudes = Await.result(lagoFuturo, Duration.Inf)._1
                ordendesConstruccion = Await.result(lagoFuturo, Duration.Inf)._2
                recursosActuales = Await.result(lagoFuturo, Duration.Inf)._3
                opcionSolicitud = Await.result(lagoFuturo, Duration.Inf)._4

              case "3" => //Cancha Futbol
                val canchaFutbolFuturo = Future {
                  Validaciones.manejoOpcionesSolicitudes(listaSolicitudes, ordendesConstruccion, recursosActuales, CanchaFutbol)
                }
                diaSimulado = diaSimulado.plusHours(24)

                listaSolicitudes = Await.result(canchaFutbolFuturo, Duration.Inf)._1
                ordendesConstruccion = Await.result(canchaFutbolFuturo, Duration.Inf)._2
                recursosActuales = Await.result(canchaFutbolFuturo, Duration.Inf)._3
                opcionSolicitud = Await.result(canchaFutbolFuturo, Duration.Inf)._4

              case "4" => //Edificio
                val edificioFuturo = Future {
                  Validaciones.manejoOpcionesSolicitudes(listaSolicitudes, ordendesConstruccion, recursosActuales, Edificio)
                }
                diaSimulado = diaSimulado.plusHours(24)

                listaSolicitudes = Await.result(edificioFuturo, Duration.Inf)._1
                ordendesConstruccion = Await.result(edificioFuturo, Duration.Inf)._2
                recursosActuales = Await.result(edificioFuturo, Duration.Inf)._3
                opcionSolicitud = Await.result(edificioFuturo, Duration.Inf)._4

              case "5" => //Gimnasio
                val gimnasioFuturo = Future {
                  Validaciones.manejoOpcionesSolicitudes(listaSolicitudes, ordendesConstruccion, recursosActuales, Gimnasio)
                }
                diaSimulado = diaSimulado.plusHours(24)

                listaSolicitudes = Await.result(gimnasioFuturo, Duration.Inf)._1
                ordendesConstruccion = Await.result(gimnasioFuturo, Duration.Inf)._2
                recursosActuales = Await.result(gimnasioFuturo, Duration.Inf)._3
                opcionSolicitud = Await.result(gimnasioFuturo, Duration.Inf)._4

              case _ => println("Opcion no valida")
                opcionSolicitud = "0"
            }
          }

        case "2" => //2. Consultar fecha de terminacion del proyecto de la ciudadela
          val fechaTerminacionFuturo = Future{
            Validaciones.consultarFechaCiudadela(ordendesConstruccion)
          }
          Await.result(fechaTerminacionFuturo, Duration.Inf)

        case "3" => //3. Informe solicitudes pendientes
          val pendientes = Future{
            Validaciones.listaOrdenesPendientes(ordendesConstruccion, diaSimulado)
          }
          ordenesPendientes = Await.result(pendientes,Duration.Inf)

          if (ordenesPendientes.isEmpty) {
            println("No hay solicitudes en progreso")
          } else {
            val listaPendientesReal = ordenesPendientes.filter(orden => diaSimulado.toDate.before(orden.fechaInicio) && orden.estado.equals("Pendiente"))

            val filtradoPendientes = Future{
              Validaciones.mostrarInformacionFiltradaProgresoPendiente(listaPendientesReal)
            }
            Await.result(filtradoPendientes, Duration.Inf)
          }

        case "4" => //4. Informe solicitudes en progreso
          val progreso = Future {
            Validaciones.listaOrdenesPendientes(ordendesConstruccion, diaSimulado)
          }
          ordenesEnProgreso = Await.result(progreso,Duration.Inf)

          if (ordenesEnProgreso.isEmpty) {
            println("No hay solicitudes en progreso")
          } else {
            val listaEnProgresosReal = ordenesEnProgreso.filter(orden => diaSimulado.toDate.equals(orden.fechaInicio) || diaSimulado.toDate.after(orden.fechaInicio)
              && diaSimulado.toDate.before(orden.fechaFinal) && orden.estado.equals("En Progreso"))

            val filtradoEnProgreso = Future{
              Validaciones.mostrarInformacionFiltradaProgresoPendiente(listaEnProgresosReal)
            }
            Await.result(filtradoEnProgreso, Duration.Inf)
          }

        case "5" => //4. Informe solicitudes finalizadas
          val fianlizadas = Future{
            Validaciones.listaOrdenesPendientes(ordendesConstruccion, diaSimulado)
          }
          ordenesFinalizadas = Await.result(fianlizadas, Duration.Inf)

          if (ordenesFinalizadas.isEmpty) {
            println("No hay solicitudes en progreso")
          } else {
            val listaFinalizadaReal = ordenesFinalizadas.filter(orden => diaSimulado.toDate.after(orden.fechaFinal) && orden.estado.equals("Finalizado"))
            
            val filtradoFinaliazadas = Future {
              Validaciones.mostrarInformacionFiltradaFinalizada(listaFinalizadaReal)
            }
            Await.result(filtradoFinaliazadas, Duration.Inf)
          }

        case _ => println("Opción no válida")
      }
    }
  }
}
