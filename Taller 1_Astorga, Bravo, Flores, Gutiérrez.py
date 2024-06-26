#####################################################################
## Taller Evaluado N°1: Herramientas Estadísticas y Forecast (HEF) ## 
#####################################################################

## Integrantes: Charo Astorga, Nathaly Bravo, Camila Flores, Nicolás Gutiérrez.


# Primero, importaremos todas las librerías que utilizaremos en este taller:

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import norm
import numpy as np

### Pregunta 1 ###

# 1.1. Cargue la información de los vinos portugueses en el programa de su preferencia.

data = pd.read_excel('Taller1.xlsx')

# 1.2. Verifique que la información se ha leído correctamente y que las variables tienen el tipo correspondiente.

#Accedemos a las cinco primeras observaciones
data.head()  

#Chequeamos el tipo de las variables
display(data.info()) 

# 1.3. Construya una tabla que muestre la cantidad de vinos blancos y tintos, además de la acidez fija promedio para cada uno de estos grupos.


#Agrupamos la base de datos con la variable vino con sus respectivas cantidades (blanco y tinto) y también calculamos el promedio de acidez fija.
tabla = data.groupby('vino').agg(
tabla = data.groupby('vino').agg( 
    acidez_fija_promedio=('acidez fija', 'mean')
).reset_index() 

#Visualizamos la tabla creada.
display(tabla)

# 1.4. Entregue también la cantidad de grados de alcohol promedio en todos los vinos presentes en la base de datos.

prom_alcohol = data['alcohol'].mean()
print("Promedio de grados en todos los vinos:",prom_alcohol.round(2))


### Pregunta 2 ###

# 2.1. Realice un histograma de densidad para el pH de todos los vinos.

#Creamos el histograma con base de datos y variables pH de todos los vinos.
plt.hist(data['pH'], bins=int(180/5), color='#A49AFF',edgecolor='#3B25FF')
plt.title('Histograma de densidad para el pH de todos los vinos')
plt.xlabel('pH')
plt.ylabel('Frecuencia')
plt.show()

# 2.2. Comente acerca de la forma del histograma e indique si tiene una forma similar a alguna(s) densidad(es) conocida(s)

#Calculamos la simetría del pH
pHsimetria = st.skew(data['pH'])
print("Coeficiente de simetría del pH:",pHsimetria)

# Calculamos la curtosis del pH
pHcurtosis = st.kurtosis(data['pH'])
print("Coeficiente de curtosis del pH:",pHcurtosis)

# 2.3. Estime por alguno de los métodos vistos en clases (momento o verosimilitud) la(s) densidad(es) propuesta(s) y superponga sobre el histograma de densidad.

#Calculamos la estimación de máxima verosimilitud
mu_mle, sigma_mle = norm.fit(data['pH']) 

#Creamos el histograma con los datos
plt.hist(data['pH'], bins=int(180/5), density=True, alpha=0.6, color='#d4ffea', edgecolor='#13EAC9') 

#Superponemos sobre la densidad estimada
xmin, xmax = plt.xlim() 
x = np.linspace(xmin, xmax, 100)
p = norm.pdf(x, mu_mle, sigma_mle)
plt.plot(x, p, '#069AF3', linewidth=1.5, label='Densidad estimada (normal)')

#Agregamos etiquetas y título al gráfico
plt.title('Histograma de densidad')
plt.xlabel('pH')
plt.ylabel('Densidad')
plt.legend()
plt.show()

### Pregunta 3 ###

# 3.1. Construya una variable que indique si cada uno de los vinos presentes en la base de datos es bueno o regular.

#Creamos una funcion para clasificar los vinos
def clasificarvino(clasificacion): 
    if clasificacion>= 6:
        return "Bueno"
    else:
        return "Regular"

# Creamos una variable a los datos y utilizamos la funcion creada para adjuntarla.
data['clasificacion'] = data['calidad'].apply(clasificarvino)

#Mostrar data creada
print(data)

# 3.2. Realice un boxplot que permita comparar el grado de alcohol entre los vinos buenos y regulares.

plt.figure(figsize=(10, 6))
plt.boxplot([data[data['clasificacion'] == 'Bueno']['alcohol'], data[data['clasificacion'] == 'Regular']['alcohol']],
            labels=['Bueno', 'Regular'], palette=['#4A65FF'], linecolor='000000')
plt.xlabel('Clasificación de vinos')
plt.ylabel('Grado de alcohol (%)')
plt.title('Comparación de grado de alcohol entre vinos buenos y regulares')
plt.show()

### Pregunta 4 ###

# 4.1.  Construya una tabla de frecuencias con la variable vino y la creada en la pregunta anterior.

# Creamos tabla de frecuencias.
tabla_frecuencias = pd.crosstab(index=data['vino'], columns=data['clasificacion'],margins_name='Total',margins ='True')

# Mostramos tabla creada de frecuencia.
display(tabla_frecuencias)


# 4.2. Considerando los porcentajes empíricos obtenidos como estimaciones de probabilidades, en el caso que seleccione al azar una botella de esta población de vinos, 
#¿cuál sería la probabilidad que la botella seleccionada corresponda a un vino tinto bueno?

# Creamos tabla de frecuencias.
tabla_frecuencias = pd.crosstab(index=data['vino'], columns=data['clasificacion'],margins_name='Total',margins ='True')

# Mostramos tabla creada de frecuencia.
display(tabla_frecuencias)


# 4.3. Si se selecciona una botella de vino blanco, ¿qué probabilidad hay de que corresponda a un vino regular?.

#Creamos la probabilidad del vino blanco
vinoblancototal = tabla_frecuencias.loc['blanco', 'Total']
vinoregularblanco = tabla_frecuencias.loc['blanco', 'Regular']

#Creamos la probabilidad del vino blanco
Probabilidadvinoblancoregular = vinoregularblanco / vinoblancototal

#Mostrar Probabilidad de vino regular blanco / vino blanco total
print("Probabilidad vino regular blanco:",Probabilidadvinoblancoregular)


### Pregunta 5 ###

#  Considere que la información entregada corresponde a todos los vinos de la última temporada de producción. 
# ¿Cuál es el valor del pH promedio de los vinos de la última producción?.

print("Media pH vinos:", data["pH"].mean())


### Pregunta 6 ###

# 6.1. En base a su muestra, encuentre la estimación máximo verosímil de la variable pH promedio de los vinos y su respectivo error estándar.

#Plantamos una semilla para garantizar reproducibilidad
np.random.seed(2467) 

#Extraemos una muestra aleatoria de 100 observaciones
muestra = data['pH'].sample(n=100, random_state=2467) 

#Calculamos la estimación de máxima verosimilitud (MLE) para la media y su error estándar
mu_muestra_mle, sigma_muestra_mle = norm.fit(muestra) 
error_estandar = sigma_muestra_mle / np.sqrt(len(muestra)) 

#Visualizamos
print(f"Estimación de máxima verosimilitud (MLE) de la media: {mu_muestra_mle}")
print(f"Error estándar: {error_estandar}")

# 6.2. Compare este valor con el que obtuvo en la pregunta anterior.

#Comparamos el valor obtenido en la pregunta anterior
mu_poblacion_mle, sigma_poblacion_mle = norm.fit(data['pH']) 
print(f"Estimación de máxima verosimilitud (MLE) de la media para toda la población: {mu_poblacion_mle}")


#Graficamos
plt.hist(muestra, bins=int(180/5), density=True, alpha=0.6, color='#d4ffea', edgecolor='#13EAC9', label='Datos de la muestra') 
xmin, xmax = plt.xlim()
x = np.linspace(xmin, xmax, 100)
p = norm.pdf(x, mu_muestra_mle, sigma_muestra_mle)
plt.plot(x, p, '#069AF3', linewidth=1.5, label='Ajuste normal (muestra)')
plt.title('Histograma de densidad (Muestra)')
plt.xlabel('pH')
plt.ylabel('Densidad')
plt.legend()
plt.show()


### Pregunta 7 ###

# 7.1. Si se calcula la desviación estándar del pH de los vinos de la última temporada, ¿este valor corresponde a un parámetro o estadístico? Justifique.

print("Desviación estándar del pH de todos los vinos:", data["pH"].std())


# 7.2. Tomando en cuenta esta información, construya un intervalo de 95% de confianza para el pH medio. Interprete.

# Paso 0: Describir parámetro de interés

#mu = Promedio del pH de todos los vinos de la última temporada


# Paso 1: Definir los datos muestrales

# data = ['pH']


# Paso 2: Definir los parámetros y criterios del intervalo de confianza

confianza = 0.95 # Nivel de confianza del 95%

nivel_confianza = stats.norm.ppf(1 - (1 - confianza) / 2) # Valor crítico de Z para el nivel de confianza


# Paso 3: Calcular estadísticas muestrales

pH_promedio = np.mean(data['pH']) # Promedio del pH

pH_desviacion_estandar = np.std(data['pH'], ddof=1) # Desviación estándar del pH (usamos ddof=1 para ajustar por muestras)


# Paso 4: Calcular el intervalo de confianza

error_estandar = pH_desviacion_estandar / np.sqrt(len(data['pH'])) # Error estándar de la media

intervalo_superior = pH_promedio + nivel_confianza * error_estandar

intervalo_inferior = pH_promedio - nivel_confianza * error_estandar


# Paso 5: Presentar los resultados

print(f"Intervalo de confianza del {confianza * 100}% para el promedio del pH de los vinos:")

print(f"({intervalo_inferior:.6f}, {intervalo_superior:.6f})")


# Paso 6: Conclusiones

print(f"Con un {confianza * 100}% de confianza, se puede afirmar que el promedio del pH de los vinos está entre")

print(f"{intervalo_inferior:.6f} y {intervalo_superior:.6f}.")





