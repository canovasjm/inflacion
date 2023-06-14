# Inflación Argentina
Este repositorio contiene una Shiny App que permite visualizar la inflación acumulada en Argentina entre dos meses definidos por el usuario.  
Link: https://canovasjm.shinyapps.io/inflación/  

### Calculo  
Para calcular la inflación acumulada en un mes específico, se usa la siguiente ecuación:  

$$inflación \ acumulada = [(1 + inflación \ en \ mes \ 1) \times \dots \times (1 + inflación \ en \ mes \ n)] - 1$$

Donde $n$ es el número de meses del periodo bajo análisis. Por ejemplo, si se quiere calcular la inflación entre enero y abril de 2023, el valor de $n$ será 4.

$$inflación \ acumulada = [(1 + 0.060) \times (1 + 0.066) \times (1 + 0.077) \times (1 + 0.084)] - 1$$  

$$inflación \ acumulada = 0.3192$$  

$$inflación \ acumulada = 31.92\%$$  

### Fuente  
INDEC, Dirección Nacional de Estadísticas de Precios, Dirección de Índices de Precios de Consumo.   
Link: https://www.indec.gob.ar/indec/web/Nivel4-Tema-3-5-31

Período de referencia: Diciembre 2016=100 
