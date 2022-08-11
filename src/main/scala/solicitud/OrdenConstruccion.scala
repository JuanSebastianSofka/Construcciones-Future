package solicitud

import tiposconstrucciones.TiposConstrucciones

import java.util.Date

case class OrdenConstruccion(
                              tiposConstrucciones: TiposConstrucciones,
                              estado: String,
                              fechaInicio: Date,
                              fechaFinal: Date,
                            )
