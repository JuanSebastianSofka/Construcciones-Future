package solicitud

import tiposconstrucciones.TiposConstrucciones

import java.util.Date

case class SolicitudConstruccion(
                                  tipoConstruccion: TiposConstrucciones,
                                  coordenadaX: Double,
                                  coodernadaY: Double,
                                  fecha: Date,
                                 /* diaSolicitud: Int,
                                  mesSolicitud: Int,
                                  anioSolicitud: Int*/
                                )
