package validaciones

import ciudadela.Ciudadela
import recursos.RecursosMateriales
import solicitud.{OrdenConstruccion, SolicitudConstruccion}
import tiposconstrucciones.TiposConstrucciones
import org.joda.time.DateTime

import java.util.{Calendar, Date}
import scala.io.StdIn.readLine

object Validaciones {

  def verificarCoordenadas(solicitudesExistentes: List[SolicitudConstruccion], nuevaSolicitud: SolicitudConstruccion): Boolean = {
    //se usa var para poder identificar sí o sí en que momento hay una solicitud con las mismas coordenadas
    var verificacion = false

    solicitudesExistentes.foreach(solicitud => {
      if (solicitud.coordenadaX == nuevaSolicitud.coordenadaX
        && solicitud.coodernadaY == nuevaSolicitud.coodernadaY) {
        verificacion = true
      }
    })
    verificacion
  }

  def verificarRecursos(recursosActuales: RecursosMateriales, solicitud: SolicitudConstruccion): Boolean = {
    //se usa var para poder identificar sí o sí que hay recursos disponibles
    var verificacion = false

    if (recursosActuales.cemento < solicitud.tipoConstruccion.cemento ||
      recursosActuales.grava < solicitud.tipoConstruccion.grava ||
      recursosActuales.arena < solicitud.tipoConstruccion.arena ||
      recursosActuales.madera < solicitud.tipoConstruccion.madera ||
      recursosActuales.adobe < solicitud.tipoConstruccion.adobe
    ) {
      verificacion = true
    }
    verificacion
  }

  def cantidadActualRecursos(verificacion: Boolean, recursos: RecursosMateriales, solicitud: SolicitudConstruccion): RecursosMateriales = {
    //se usa var para poder retornar nuevos amteriales
    var nuevosRecursos: RecursosMateriales = null

    if (!verificacion) {
      val cemento = recursos.cemento - solicitud.tipoConstruccion.cemento
      val grava = recursos.grava - solicitud.tipoConstruccion.grava
      val arena = recursos.arena - solicitud.tipoConstruccion.arena
      val madera = recursos.madera - solicitud.tipoConstruccion.madera
      val adobe = recursos.adobe - solicitud.tipoConstruccion.adobe

      nuevosRecursos = RecursosMateriales(
        cemento = cemento,
        grava = grava,
        arena = arena,
        madera = madera,
        adobe = adobe
      )
    } else {
      nuevosRecursos = recursos
    }
    nuevosRecursos //puede llegar a tener numeros negativos por lo tanto se valida con ello si no hay recursos
  }

  def manejoOpcionesSolicitudes(listaSolicitudes: List[SolicitudConstruccion], ordendesConstruccion: List[OrdenConstruccion], recursosActuales: RecursosMateriales, construccion: TiposConstrucciones):
  (List[SolicitudConstruccion], List[OrdenConstruccion], RecursosMateriales, String) = {
    if (listaSolicitudes.isEmpty) {
      println("Ingrese la coordenada X donde se localizará la construcción")
      val coorX = readLine()

      println("Ingrese la coordenada Y donde se localizará la construcción")
      val coorY = readLine()

      val solicitud = SolicitudConstruccion(tipoConstruccion = construccion,
        coordenadaX = coorX.toInt, coodernadaY = coorY.toInt,
        fecha = Calendar.getInstance().getTime)

      val fechaInicioOrden = new Date(solicitud.fecha.getTime + (1000 * 60 * 60 * 24)) //dia de mañana
      val fechaFinalOrden = new Date(fechaInicioOrden.getTime + ((1000 * 60 * 60 * 24) * (solicitud.tipoConstruccion.diasConstruccion + 1)))

      val ordenPrueba = OrdenConstruccion(
        solicitud.tipoConstruccion,
        "pendiente",
        fechaInicioOrden,
        fechaFinalOrden
      )

      val solicitudesActualizadas = listaSolicitudes :+ solicitud
      val ordenesActualizadas = ordendesConstruccion :+ ordenPrueba
      val recursosActualizados = Validaciones.cantidadActualRecursos(verificacion = false, recursos = recursosActuales, solicitud = solicitud)

      println(
        """
          |Se ha ingresado la solicitud con exito
          |Se ha creado la orden de construccion con exito
          |""".stripMargin)

      val opcionSolicitud = "0"

      (solicitudesActualizadas, ordenesActualizadas, recursosActualizados, opcionSolicitud)
    } else {

      println("Ingrese la coordenada X donde se localizará la construcción")
      val coorX = readLine()

      println("Ingrese la coordenada Y donde se localizará la construcción")
      val coorY = readLine()

      val solicitud = SolicitudConstruccion(tipoConstruccion = construccion,
        coordenadaX = coorX.toInt, coodernadaY = coorY.toInt,
        fecha = Calendar.getInstance().getTime)

      //si alguna coordenada de la lista de solicitudes coincide con la nueva solicitud retorna true
      val verificarCoor = Validaciones.verificarCoordenadas(listaSolicitudes, solicitud)

      //si por algun motivo los recursos son menores a lo que necesito retorna true
      val verificarRec = Validaciones.verificarRecursos(recursosActuales, solicitud)

      if (!verificarRec && !verificarCoor) {

        val solicitudesActualizadas = listaSolicitudes :+ solicitud
        val fechaInicioOrden = new Date(ordendesConstruccion.last.fechaFinal.getTime + (1000 * 60 * 60 * 24))
        val fechaFinalOrden = new Date(fechaInicioOrden.getTime + ((1000 * 60 * 60 * 24) * (solicitud.tipoConstruccion.diasConstruccion + 1)))

        val ordenPrueba = OrdenConstruccion(
          solicitud.tipoConstruccion,
          "pendiente",
          fechaInicioOrden,
          fechaFinalOrden
        )

        //añado la orden con la fecha "movida" según si hay solicitudes previas
        val ordenesActualizadas = ordendesConstruccion :+ ordenPrueba

        //recalculo los recursos
        val recursosActualizados = Validaciones.cantidadActualRecursos(verificacion = false, recursos = recursosActuales, solicitud = solicitud)

       println(
          """
            |Se ha ingresado la solicitud con exito
            |Se ha creado la orden de construccion con exito
            |""".stripMargin)
        val opcionSolicitud = "0"

        (solicitudesActualizadas, ordenesActualizadas, recursosActualizados, opcionSolicitud)

      } else {
        println(
          """Las coordenadas ingresadas coinciden con alguna coordenada existe
            |o los recursos no son suficientes y por ende no podemos aceptar la
            |solicitud
            |""".stripMargin)

        val opcionSolicitud = "0"

        (listaSolicitudes, ordendesConstruccion, recursosActuales, opcionSolicitud)
      }
    }
  }

  def consultarFechaCiudadela(ordendesConstruccion: List[OrdenConstruccion]): Unit = {
    if (ordendesConstruccion.isEmpty) {
      println("No hay órdenes a realizar, por lo tengo no hay fecha de culminación para el proyecto")
      println("")
    } else {
      val fechaTerminacionCiudadela = new Date(ordendesConstruccion.last.fechaFinal.getTime)
      val ciudadela = Ciudadela(fechaTerminacionCiudadela)
      println(s"El proyecto se espera estar terminado el dia ${ciudadela.fechaTerminacion}")
      println("")
    }
  }

  def listaOrdenesPendientes(ordendesConstruccion: List[OrdenConstruccion], diaSimulado: DateTime): List[OrdenConstruccion] = {

    val listaPendientes: List[OrdenConstruccion] = ordendesConstruccion.map(construccion => {
      if(diaSimulado.toDate.before(construccion.fechaInicio)){
        val ordenPendiente = OrdenConstruccion(
          construccion.tiposConstrucciones,
          "Pendiente",
          construccion.fechaInicio,
          construccion.fechaFinal
        )
        ordenPendiente
      }else if(diaSimulado.toDate.after(construccion.fechaInicio) && diaSimulado.toDate.before(construccion.fechaFinal)){
        val ordenEnProgreso = OrdenConstruccion(
          construccion.tiposConstrucciones,
          "En Progreso",
          construccion.fechaInicio,
          construccion.fechaFinal
        )
        ordenEnProgreso
      }else if(diaSimulado.toDate.after(construccion.fechaFinal)){

        val ordenEnProgreso = OrdenConstruccion(
          construccion.tiposConstrucciones,
          "Finalizado",
          construccion.fechaInicio,
          construccion.fechaFinal
        )
        ordenEnProgreso
      }
      else{
        construccion
      }
    })
    listaPendientes
  }

  def mostrarInformacionFiltradaProgresoPendiente(orden: List[OrdenConstruccion]): Unit ={
    if(orden.isEmpty){
      println("Aun no hay información para ésta solicitud")
    }else{
      orden.foreach(o => println(
        s"""Información órden:
           |Tipo: ${o.tiposConstrucciones.nombre}
           |Estado: ${o.estado}
           |Fecha Inicio Obra: ${o.fechaInicio}
           |""".stripMargin
      ))
    }
  }

  def mostrarInformacionFiltradaFinalizada(orden: List[OrdenConstruccion]): Unit ={
    if (orden.isEmpty) {
      println("Aun no hay información para ésta solicitud")
    } else {
      orden.foreach(o => println(
        s"""Información órden
           |Tipo: ${o.tiposConstrucciones.nombre}
           |Estado: ${o.estado}
           |Fecha Final Obra: ${o.fechaFinal}
           |""".stripMargin))
    }
  }
}
