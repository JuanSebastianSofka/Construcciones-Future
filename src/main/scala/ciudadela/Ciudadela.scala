package ciudadela

import solicitud.OrdenConstruccion

import java.util.Date

case class Ciudadela(
                      /*ordenesPendientes: List[OrdenConstruccion], //depende del estado "pendiente"
                      ordenesTerminadas: List[OrdenConstruccion], //depende del estado "terminado"
                      ordenesEnProgreso: List[OrdenConstruccion], //depende del estado "en progreso"*/
                      fechaTerminacion: Date,
                    )
