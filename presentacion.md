# Presentación

Introducción muy abstracta y corta de blockchain o dar la mirada desde otro lado.
Hablar de un smart contract y vincular con Plutus, hablar de las caracteristicas,
las mónadas ox que tienen, qué es lo especial que tiene. O quizá hablando el
problema y porqué se eligió programación funcional para el lenguaje de los contratos.

Plutus es muy parecido a haskell por lo tanto no hay que explicarlo tanto.

Charlas que presenta la gente, Chacrabarsky, Tompson, abstractas.
IOHK -> articulos.
https://docs.cardano.org/plutus/learn-about-plutus

NO intentar programar un smart contract.
https://iohk.io/en/research/library/papers/functional-blockchain-contracts/


## Introducción

Para poder entender el tema a presentar primero hay que conocer ciertos conceptos, en especial hay que tener una noción de lo que es blockchain y lo que es un smart contract.

Blockchain es una manera de guardar información de tal manera que sea dificil de cambiar o hackear. Una blockchain es esencialmente un registro de transacciones, el cual es replicado y distribuido sobre toda la red de computadoras conectadas a la blockchain. Cada bloque de la cadena contiene información sobre las transacciones realizadas.

Tener una blockchain garantiza seguridad ya que cada bloque para ser validado es encriptado, además al ser un sistema distribuido es dificil que colapse. Tiene varias otras caracteristicas más aparte de lo mencionado anteriormente, pero en lo que nos vamos a centrar en esta presentación es en que la blockchain es programable, por ejemplo se pueden crear "smart contracts".

¿Que es un smart contract?

Son programas que son guardados en la blockchain y cuando se cumplen ciertas condiciones se ejecutan automaticamente. Tipicamente son utilizados para asegurarse que al hacer una transacción, todos los participantes de la misma reciban lo que se prometio, sin necesitar de un intermediario, otro uso posible es para automatizar tareas, por ejemplo, se puede programar una subasta de un NFT.

El tema que vamos a presentar es un lenguaje llamado Plutus implementado a través de Haskell, el cual es utilizado para la manipulación de "smart contracts" en una blockchain llamada Cardano. 

## Ver en que parte mechar esta info
- Cometer un error en un smart contract es terriblemente costoso ya que todo lo que entra a la blockchain es inmutable, por lo tanto no es posible corregir un error dentro de ese contrato. Para liberar un smart contract es recomendable realizar una gran cantidad de pruebas, para asegurar el funcionamiento correcto del mismo. Por esta razon se utiliza un lenguaje de programación funcional, el cual permite realizar pruebas de manera matematica y poder asegurarme que si tengo las mismas entradas siempre voy a obtener la misma salida. 


- Plutus is Readable, predictable and mathematicly provable programming language
- high assurance level of code predictibility 
- 

## Plataforma Plutus

Bitcoin tiene un soporte mínimo de validaciones personalizadas, por lo tanto está restringido a proveer nada mas que una funcionalidad simple de contabilidad.

Ethereum brinda un lenguage de programación de uso general llamado Solidity que permite reglas de validacion arbitrariamente complejas, el tema es que esta expresividad viene con el costo de un modelo computacional semanticamente complejo, donde se favorecen los modelos de programación orientada a objetos, los cuales introducen estados mutables compartidos a una red que ya es concurrente y distribuída. 

## Por qué Haskell?

Además ambos dependen de nuevos lenguajes especificos que requieren nuevas librerías, material educativo, comunidades y demás, como consecuencia es dificil razonar los comportamientos de las aplicaciones resultantes, lo que resulta en una cantidad de vulnerabilidades.

## Arquitectura

Uno es dueño de una cantidad de divisas si tiene la clave privada que matchea con la clave pública que blockea la salida, y esta clave es la que le da poder para gastar la divisa, por lo tanto, una transacción que contiene un input consumidor de una salida dada en el conjunto UTxO solo será admitida si está firmada criptograficamente con la clave privada que matchea la clave pública de la misma.

Grafo aciclico.

## Transacción

El Slot count provee una noción del tiempo pasado desde la creación de la blockchain, este intervalo determina el rango de slots en el cual la transaccion será valida.

## Inputs y Outputs

TxID es el identificador único de la transacción.

## Representación Plutus Core

El código on chain es enviado dentro de una transacción, y una vez que lleag a la blockchain es inmutable.

## Playground

Una de las ventajas que tiene programar smart contracts con Plutus es que tiene un playground donde se puede probar el código con una interfaz bastante amigable y fácil de utilizar. En donde se puede probar el código de una manera un poco más interactiva. Dentro del playground hay varios ejemplos de smart contracts implementados.

Por ejemplo vamos a mostrar como se vería una transacción sencilla de cryptomonedas de una billetera electronica a otra. Este es el caso más sencillo.

Por otro lado también se podría hacer algo un poco más complejo como 
