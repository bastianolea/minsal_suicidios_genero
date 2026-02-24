# Casos de intento de suicidio y suicidio consumado en Chile

[Visualizaciones de datos](https://bastianolea.github.io/minsal_suicidios_genero/) con perspectiva de género sobre suicidios, obtenidos desde el Ministerio de Salud de Chile, en base al egreso con o sin vida de cada paciente, cruzando los egresos hospitalarios con los códigos de egreso relacionados a lesiones autoinflingidas intencionalmente.

> Si estás pasando por un momento difícil, o estás teniendo ideas suicidas, **por favor busca ayuda**. El suicidio nunca es la solución, y te aseguro que todo va a mejorar.

- _Línea de prevención del suicidio (Chile):_ llama al *4141 a cualquier hora del día.
- _Fono de orientación en violencias de género:_ llama al 1455


## Aplicación
Visualizaciones de datos y estadísticas disponibles en: [https://bastianolea.github.io/minsal_suicidios_genero/](https://bastianolea.github.io/minsal_suicidios_genero/)


## Datos

Los datos se descargan desde [Datos abiertos (DEIS)](https://deis.minsal.cl/#datosabiertos) en archivos csv por año, los cuales se guardan en la carpeta `datos`. Estos archivos son cargados en el script `procesar.R`, el cual los limpia y luego los une en una sola base de datos.

Descarga los datos procesados:

- [Egresos de pacientes, por género, por intento de suicidio o suicidio consumado](https://github.com/bastianolea/minsal_suicidios_genero/raw/master/datos/minsal_suicidios.parquet)
- [Egresos de pacientes, por género, por intento de suicidio o suicidio consumado, y con información de diagnóstico](https://github.com/bastianolea/minsal_suicidios_genero/raw/master/datos/minsal_suicidios_diagnostico.parquet)

### Actualizaciones
- 2026/02/24: datos actualizados para incluir 2018, 2018 y 2019. Se adecúan los resultados para considerar el rol de la pandemia. Gracias a Belén Vargas Gallegos por sus comentarios y correcciones.
- 2026/02/22: lanzamiento de primera versión.
- 2025/11/11: primeras exploraciones de los datos (luego sin actividad hasta 2026/02/20)

### Fuente 
- Ministerio de Salud, Egresos hospitalarios
- Datos abiertos, Departamento de Estadísticas e Información de Salud: https://deis.minsal.cl/#datosabiertos


----

Desarrollado por [Bastián Olea Herrera](https://bastianolea.rbind.io) con el lenguaje de programación R. Código disponible en [GitHub](https://github.com/bastianolea/minsal_suicidios).