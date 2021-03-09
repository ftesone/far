# FAR

Calculadora de Álgebra Relacional que utiliza formato csv para entrada y salida

### Contenido

1. [Introducción](#1-introducción)

2. [Compilación](#2-compilación)

3. [Uso](#3-uso)

    3.1. [Ejecución](#31-ejecución)

    3.2. [Operaciones](#32-operaciones)

4. [Ejemplos](#4-ejemplos)

5. [Sintaxis EBNF](#5-sintaxis-ebnf)

## 1. Introducción



## 2. Compilación

Para compilar, debe ejecutarse desde terminal:

```bash
ghc far.hs
```

Este comando genera un archivo con nombre `far` ejecutable.

## 3. Uso

### 3.1. Ejecución

El programa debe ejecutarse proveyendo dos parámetros:

 1. Path a directorio con archivos CSV, donde cada archivo debe cumplir con las siguientes restricciones:
    - debe tener extensión `.csv`
    - debe utilizar el caracter `;` como separador
    - la primera línea del archivo debe contener los nombres de las columnas
    - todas las líneas del archivo debe tener la misma cantidad de columnas
    - la codificación utilizada debe ser UTF-8
    - los saltos de línea utilizados deben ser Unix (`LF`)
 1. String cuyo valor representa la fórmula de álgebra relacional (AR) a resolver, donde:
    - se debe definir al menos una relación a utilizar, considerando que los nombres de las relaciónes se corresponden con los nombres de los archivos sin la extensión (es decir, si dentro del directorio que se referencia como primer parámetro existe un archivo con nombre `prueba.csv`, entonces en las fórmulas de AR puede utilizarse la relación `prueba`).
    - se utilizan los siguientes símbolos para las operaciones:
        - `σ` para la operación de selección
        - `π` para la operación de proyección
        - `✕` para la operación de producto cartesiano
        - `⋈` para la operación de producto natural
        - `∪` para la operación de unión
        - `∩` para la operación de intersección
        - `−` para la operación de diferencia

### 3.2. Operaciones

Las **operaciones unarias** se definen escribiendo `símbolo [parámetro] fórmula`, donde `símbolo` es el operador (`σ` o `π`), `parámetro` (nótese que se delimita con corchetes) representa una fórmula booleana para la selección o una lista de atributos para la proyección, y `fórmula` una fórmula de AR.

Las fórmulas booleanas se definen comparando el valor de un atributo con otro, o el valor de un atributo con un valor constante, utilizando los operadores `=` (igual), `≠` (distinto), `<` (menor), `≤` (menor o igual), `>` (mayor), `≥` (mayor o igual). Es posible definir fórmulas compuestas con los operadores `~` (negación), `∨` (disyunción), `∧` (conjunción).

Las **operaciones binarias** se definen escribiendo `fórmula símbolo fórmula`, donde `símbolo` es el operador (`✕`, `⋈`, `∪`, `∩` o `−`), y `fórmula` una fórmula de AR.

La fórmula AR mínima es el nombre de una relación.

La precedencia de las operaciones está definida de la siguiente forma:
 - las operaciones unarias tienen mayor precedencia que las binarias;
 - las operaciones de la izquierda tienen mayor precedencia que las de la derecha;
 - la precedencia puede modificarse utilizando paréntesis

## 4. Ejemplos

A partir de la siguiente estructura de archivos:

```
/ejemplo
    alumnos.csv
    aprobados.csv
    materias.csv
far
```

El siguiente contenido de archivos:

*alumnos.csv*:
```csv
legajo;apellido_nombre;email
69827/2;Merino, Carlos;cmerino@mail.com
55044/1;Barrionuevo, Belén;bbarrionuevo@mail.com
39965/4;Sánchez, Belén;bsanchez@mail.com
42617/2;Oberto, Noel;noberto@mail.com
97520/7;Barrionuevo, Delfina;dbarrionuevo@mail.com
85156/5;Rognoni, Gabriel;grognoni@mail.com
```

*materias.csv*:
```csv
idMateria;nombre
1;Conceptos de Algoritmos, Datos y Programas
2;Organización de computadoras
3;Matemática 1
4;Taller de programación
5;Arquitectura de computadoras
6;Matemática 2
```

y *aprobados.csv*:
```csv
legajo;idMateria;nota
69827/2;1;8
55044/1;1;7
39965/4;1;8
42617/2;1;9
97520/7;1;5
69827/2;2;4
42617/2;2;7
97520/7;2;10
69827/2;3;6
55044/1;3;6
39965/4;3;8
42617/2;3;5
```

Las consultas que se desean resolver:

1. Listar legajo, apellido y nombre, y email de alumnos que no aprobaron ninguna materia.
2. Nombre y nota de materias que aprobó el alumno "Merino, Carlos", con legajo 69827/2.
3. Nombre de materias aprobadas por "Oberto, Noel" y por "Barrionuevo, Delfina".

Ejemplo de cómo se resuelven las consultas:

1. `./far ejemplo "alumnos − π [legajo, apellido_nombre, email] (alumnos ⋈ aprobados)"`
2. `./far ejemplo "π [nombre] (materias ⋈ aprobados ⋈ σ [apellido_nombre=\"Merino, Carlos\" ∧ legajo=\"69827/2\"] alumnos)"`
3. `./far ejemplo "π [nombre] (materias ⋈ aprobados ⋈ σ [apellido_nombre=\"Oberto, Noel\"] alumnos) ∩ π [nombre] (materias ⋈ aprobados ⋈ σ [apellido_nombre=\"Barrionuevo, Delfina\"] alumnos)"`

Con los datos de ejemplo, la salida de la primera consulta es:

```
legajo;apellido_nombre;email
85156/5;Rognoni, Gabriel;grognoni@mail.com
```

**Nota:** los datos de personas utilizados en el ejemplo corresponden a:
 - legajos generados aleatoriamente
 - nombres y apellidos seleccionados aleatoriamente a partir de un conjunto arbtrario

## 5. Sintaxis EBNF

```ebnf
formula = identificador
        | open_p , formula , close_p
        | operador_ar_proyeccion, {sp}, lista_atributos, {sp}, formula
        | operador_ar_seleccion, {sp}, expresion_bb, {sp}, formula
        | formula, {sp}, operador_ar_binario, {sp}, formula

lista_atributos = open_c , {sp} , identificador , { coma , {sp} , identificador } , {sp} , close_c ;

expresion_bb = expresion_bbs | open_p , {sp}, expresion_bb , {sp} , close_p
expresion_bbs = expresion_b | operador_bb_unario, {sp}, expresion_bb | expresion_bb, {sp}, operador_bb_binario, {sp}, expresion_bb

expresion_b = expresion_bs | open_p , {sp} , expresion_b , {sp} , close_p
expresion_bs = identificador_atributo , {sp}, operador_b, {sp}, identificador_atributo | identificador_atributo , {sp}, operador_b, {sp}, string | string , {sp}, operador_b, {sp}, identificador_atributo ;

identificador_atributo = identificador , [ punto , identificador ] ;
identificador = minus | mayus , [{ minus | mayus | digito | "_"}] ;

operador_bb = operador_bb_unario | operador_bb_binario ;
operador_bb_binario = "∨" | "∧" ;
operador_bb_unario = "~" ;
operador_b = "<" | ">" | "=" | "≠" | "≤" | "≥" ;

operador_ar = operador_ar_unario | operador_ar_binario ;
operador_ar_binario = "∩" | "∪" | "−" | "χ" | "⋈" ;
operador_ar_unario = operador_ar_proyeccion | operador_ar_seleccion ;
operador_ar_seleccion = "σ" ;
operador_ar_proyeccion = "π" ;

string = '"', { char }, '"' ;
char = letra | letra_especial | sp | digito | simbolo ;

sp = " ";
open_c = "[" ;
close_c = "]" ;
open_p = "(" ;
close_p = ")" ;
punto = ".";
coma = "," ;

letra = minus | mayus ;
minus = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" ;
mayus = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" ;
digito = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
simbolo = "-" | "_" ;
letra_especial = "á" | "é" | "í" | "ó" | "ú" | "ü" | "ñ" | "Á" | "É" | "Í" | "Ó" | "Ú" | "Ü" | "Ñ" ;
```
