# Ciudadela Del Futuro

La empresa “Constructores S.A.S” será la encargada de construir la “Ciudadela del Futuro”. Esta empresa requiere desarrollar una aplicación capaz de gestionar sus solicitudes de construcción en el terreno designado para la ciudadela.

Las solicitudes de construcción solo serán montadas por el arquitecto.

## Materiales

Para la construcción se requieren 5 tipos de materiales diferente:

```bash
·      Cemento (Ce)
·      Grava (Gr)
·      Arena (Ar)
·      Madera (Ma)
·      Adobe (Ad)
```

## Tipos de Construcciones
En la ciudadela se podrán construir cinco diferentes tipos de construcciones, las cuales requieren una cantidad específica de material para poder construirse y llevan un tiempo específico en ser terminada:

```bash
·      Casa (Ce: 100, Gr: 50, Ar: 90, Ma: 20, Ad: 100) => 3d
·      Lago (Ce: 50, Gr: 60, Ar: 80, Ma: 10, Ad: 20) => 2d
·      Cancha de fútbol (Ce: 20, Gr: 20, Ar: 20, Ma: 20, Ad: 20) => 1d
·      Edificio (Ce: 200, Gr: 100, Ar: 180, Ma: 40, Ad: 200) => 6d
·      Gimnasio (Ce: 50, Gr: 25, Ar: 45, Ma: 10, Ad: 50) => 2d
```
Para hacer una solicitud de construcción se requiere especificar el tipo de construcción y en que coordenada (x,y) se realizará la construcción. Si se pudo realizar la solicitud, posterior a esto se crea la orden de construcción con un estado “pendiente”.

## Tiempos
La ciudadela no tiene una fecha exacta de entrega, por lo anterior cada orden de construcción ampliará el plazo de entrega de la ciudadela.

Cada construcción se arranca el día siguiente al día solicitado o al día siguiente de terminar la construcción previamente programado y termina en la noche del día de finalización calculado.

Solo se podrá llevar a cabo construcción a construcción (de manera lineal en el tiempo). Ejemplo:
```bash
· Si se solicitó el 01/01/2019 la construcción de una casa, 
esta empezaría el 02/01/2019 y terminaría el 06/01/2019

· Si se solicitó el 02/01/2019 la construcción de un lago, 
esta empezaría 07/01/2019 y terminaría el 10/01/2019
```
## Validaciones por Día
```bash
· En las mañanas se debe validar si se debe colocar en el estado “en progreso” 
alguna orden de construcción y actualizar el estado dado el caso

· En las noches se debe validar si se debe colocar en el estado “finalizado” 
alguna orden de construcción y actualizar el estado dado el caso
```

## Permisos

El sistema debe permitir hacer solicitudes de construcción así no haya terminado alguna construcción previa. El sistema solo debe restringir la solicitud de construcción si no posee los materiales y si en las coordenadas ya existe alguna orden de construcción ya sea programada, en progreso o finalizada.

El sistema debe notificar si fue posible hacer la solicitud de construcción. La creación de la orden de construcción se hace luego de notificarle al usuario que se pudo realizar la solicitud.

El sistema debe permitir consultar cual es la fecha en que termina el proyecto. El proyecto debe cambiar la fecha dependiendo de los pedidos programados.

El sistema debe permitir solicitar un informe de que construcciones están pendientes, cuantas por tipo se han terminado y cuantas en progreso.
## Importante
¿Con cuántos recursos se empieza? Con los que usted quiera

¿Se puede recargar materiales de construcción? Puede añadir funcionalidad a la aplicación para recargar los materiales

¿Los informes deben ser separados o un solo informe con toda la información? Como usted lo quiera hacer

¿En qué se debe presentar el informe? Puede ser un archivo “.txt” o como se le pueda ocurrir

¿La generación del informe debe restringir/bloquear el uso de la aplicación? No debe restringir

¿Cuándo se dice “en la mañana” y “en la noche” a que momento exacto se refiere? A una hora x cualquiera que sea en la mañana y una hora y cualquiera que sea en la noche (no es tan relevante la hora exacta), solo es importante que sea a una hora específica que se realice dicho proceso.

¿Debo hacer capa de presentación o UI? No. Si la hace, no se tiene en cuenta en la evaluación de la prueba.

¿Qué pasa si por algún error falla la creación de la orden de construcción? Se debe notificar de alguna manera que hubo un problema así sea logueando el error

## Observaciones para la solución (Nota de Juán Sebastián Franco Cervera)

Se planteó el uso de una variable para simular el paso de los días cada 24 horas por cada nueva solicitud que el usuario ingrese, para así poder cambiar el estado de las órdenes: Pendiente, En Progreso, Finalizado. Esto con el fin de a su vez 
poder filtrar la información que el usuario desee ver según el tipo de solicitud que desea realizar

Entre las solicitudes están:
0. Cancelar
1. Ingresar nueva solicitud
2. Consultar fecha de terminacion del proyecto de la ciudadela
3. Informe solicitudes pendientes
4. Informe solicitudes en progreso
5. Informe solicitudes finalizadas

Por lo tanto con el pasar del tiempo se puede ir solicitando los informes y ver la información que contienen de acuerdo al estado.

La opción 1 permitirá luego escoger una de las siguientes opciones:
0. Salir
1. Casa
2. Lago
3. Cancha de Fútbol
4. Edificio
5. Gimnasio

Con ello cada que se genere una nueva solicitud de las anteriores opciones se aumenta el dia simulado en 24 horas