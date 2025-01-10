# AEJ Dashboard

## Descripción
Dashboard interactivo para la visualización y análisis de la Encuesta de Percepción Ciudadana de Ciudad Juárez. Esta herramienta permite explorar los resultados de la encuesta a través de visualizaciones interactivas y análisis detallados por áreas temáticas.

## Características Principales
- Visualización interactiva de resultados de la encuesta
- Análisis por áreas temáticas
- Comparativas temporales
- Exportación de datos y gráficos
- Interfaz en español
- Visualizaciones responsivas

## Requisitos del Sistema
- R >= 4.1.0
- RStudio >= 1.4
- Shiny Server >= 1.5.17
- Dependencias listadas en DESCRIPTION

## Instalación

```r
# Instalar dependencias
if (!require(renv)) install.packages("renv")
renv::restore()

# Ejecutar la aplicación localmente
shiny::runApp()
```

## Estructura de Datos
Los datos de la encuesta se organizan en las siguientes categorías principales:
- Demografía
- Economía
- Salud
- Educación
- Seguridad
- Servicios Públicos
- Participación Ciudadana
- Gobierno

## Desarrollo
### Configuración del Entorno
1. Clonar el repositorio
2. Instalar dependencias con `renv::restore()`
3. Configurar variables de entorno según `.env.example`

### Flujo de Trabajo
1. Crear rama para nueva característica
2. Desarrollar y probar localmente
3. Ejecutar pruebas unitarias
4. Crear Pull Request

## Despliegue
Instrucciones detalladas para el despliegue en `docs/deployment.md`

## Licencia
Apache 2.0

## Contacto
Francisco Leyva (f.leyva@planjuarez.org)