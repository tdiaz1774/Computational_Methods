Tomas Diaz Servin A01637531

Mateo González Cosío Ríos y Valles a01023938

Jorge Cabiedes Acosta A01024053

Reflexión Actividad 3.4

Primero, para poder solucionar el primer reto del código que se debía de encargar de leer el archivo .json, implementamos una función (utilizando recursión de cola) que se encarga de leer el archivo línea por línea, que al igual que regresar una lista de listas, se  encarga de almacenar  de los diferentes caracteres que contiene cada línea. Una vez que finaliza esto el código, tomamos la lista del resultado y la pasamos por la función que se encarga de distinguir cada valor de la lista y asignarle que tipo de variable es. Para esto, empleamos el uso de la recursión de cola una vez mas (con la función loop), que pasa cada valor de la lista de listas (result) por diferentes condiciones. Estas condiciones utilizan las expresiones regulares para poder distinguir, y asignarle el tipo de valor a cada posición de la lista result. Esta parte del código retorna el valor y su tipo para que luego después lo utilice para asignarle el color del tipo de variable. Finalmente, el algoritmo se encarga de tomar la lista que retorna la función indentify-object, y por cada valor le asigna los caracteres correspondientes para ahora pasar cada línea de formato json a formato HTML en la lista json_html. Finalmente, utilizamos la función complete_file para le apendar a la lista open_html, que consiste en el header con la sintaxis apropiada de HTML donde igual se definen los colores de cada tipo de valor, a la lista json_html ya mencionada y al final el closer apropiado. El programa se concluye cuando utilizando esta misma función, se genera el archivo html. 

Podemos concluir que la complejidad de nuestro algoritmo es de O(N), que significa que el tiempo crece linealmente con el numero de iteraciones o en este caso líneas que tiene que leer el código. En el caso de nuestras pruebas, resulto que el tiempo que se tarda para correr 38 líneas fue de 1.245 segundos que concuerda con lo que podemos asumir. 

