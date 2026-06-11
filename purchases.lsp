#| 
  integrantes:
  Nicolas Rocha Gutierrez
  Jose David Hurtado Arandano
|#

; Funcion auxiliar que busca un cliente registrado por su numero de cliente
; retorna el cliente si existe, nil si no
(defun findCustomerByNumber (customerNumber)
    (setq found nil)
    (setq i 0)
    (loop
        (when (= customerNumber (Customer-customerNumber (aref customerArray i)))
            (setq found (aref customerArray i))
        )
        (setq i (+ i 1))
        (when (>= i customerCount) (return))
    )
    found
)

; Funcion auxiliar que pide y valida la fecha de compra
; retorna la fecha como string en formato DD/MM/AAAA
(defun askDate()
    (loop
        (print "Digite el dia de la compra (1-31): ")
        (setq day (read))
        (when (and (integerp day) (>= day 1) (<= day 31)) (return))
        (print "Dia invalido, debe ser un numero entre 1 y 31")
    )
    (loop
        (print "Digite el mes de la compra (1-12): ")
        (setq month (read))
        (when (and (integerp month) (>= month 1) (<= month 12)) (return))
        (print "Mes invalido, debe ser un numero entre 1 y 12")
    )
    (loop
        (print "Digite el anio de la compra (numero entero positivo): ")
        (setq year (read))
        (when (and (integerp year) (> year 0)) (return))
        (print "Anio invalido, debe ser un numero entero positivo")
    )
    (format nil "~S/~S/~S" day month year)
)

; Funcion auxiliar que muestra los libros disponibles y retorna el libro seleccionado
(defun selectBook()
    (format t "~%--- LIBROS DISPONIBLES ---~%")
    (setq i 0)
    (loop
        (setq book (aref bookArray i))
        (format t "~S. ~S - ~S - $~S~%"
            (+ i 1)
            (Book-title book)
            (Book-topic book)
            (Book-price book)
        )
        (setq i (+ i 1))
        (when (>= i bookCount) (return))
    )
    (loop
        (print "Seleccione el numero del libro a comprar: ")
        (setq bookOption (read))
        (when (and (integerp bookOption) (>= bookOption 1) (<= bookOption bookCount)) (return))
        (print "Opcion invalida, intente de nuevo")
    )
    (aref bookArray (- bookOption 1))
)

; Funcion encargada de registrar la compra de un libro con su respectivo pago
(defun buyBook()
    (if (= bookCount 0)
        (print "No hay libros disponibles")
        (progn
            (if (>= purchaseCount (length purchaseArray))
                (print "No hay espacio para mas compras")
                (progn
                    (setq purchase (make-Purchase))

                    ; preguntar si es cliente registrado
                    (loop
                        (print "Es usted un cliente registrado?")
                        (print "  1.  Si")
                        (print "  2.  No")
                        (format t "~%")
                        (print "Digite su respuesta (debe digitar el numero): ")
                        (setq isRegistered (read))
                        (when (and (integerp isRegistered) (or (= isRegistered 1) (= isRegistered 2))) (return))
                        (print "Opcion invalida, intente de nuevo")
                    )

                    (if (= isRegistered 1)
                        ; cliente registrado: buscar por numero de cliente
                        (progn
                            (loop
                                (print "Digite su numero de cliente (numero entero positivo): ")
                                (setq customerNum (read))
                                (when (and (integerp customerNum) (> customerNum 0)) (return))
                                (print "Numero de cliente invalido, debe ser un numero entero positivo")
                            )
                            (setq foundCustomer (findCustomerByNumber customerNum))
                            (if (null foundCustomer)
                                (format t "~%Cliente con numero ~S no encontrado~%" customerNum)
                                (progn
                                    (setf (Purchase-customer purchase) foundCustomer)
                                    (format t "~%Bienvenido ~S ~S~%"
                                        (Customer-name foundCustomer)
                                        (Customer-lastName foundCustomer)
                                    )
                                    ; seleccionar libro usando funcion auxiliar
                                    (setf (Purchase-book purchase) (selectBook))

                                    ; pedir fecha
                                    (setf (Purchase-date purchase) (askDate))

                                    ; asignar compra y guardar
                                    (setf (aref purchaseArray purchaseCount) purchase)
                                    (setq purchaseCount (+ purchaseCount 1))

                                    ; calcular precio con descuento del 10%
                                    (setq bookPrice (Book-price (Purchase-book purchase)))
                                    (setq discount (* bookPrice 0.1))
                                    (setq finalPrice (- bookPrice discount))

                                    ; mostrar resumen con descuento
                                    (format t "~%--- RESUMEN DE COMPRA ---~%")
                                    (format t "Libro            : ~S~%" (Book-title (Purchase-book purchase)))
                                    (format t "Fecha            : ~S~%" (Purchase-date purchase))
                                    (format t "Precio original  : $~S~%" bookPrice)
                                    (format t "Descuento (10%%) : $~S~%" discount)
                                    (format t "Total a pagar    : $~S~%" finalPrice)
                                    (format t "~%Compra realizada exitosamente!~%")
                                )
                            )
                        )
                        ; cliente no registrado: pedir datos
                        (progn
                            (setq tempCustomer (make-Customer :customerNumber nil))

                            (loop
                                (print "Digite su nombre (con comillas, ejemplo: \"Juan\"): ")
                                (setq tempName (read))
                                (when (stringp tempName) (return))
                                (print "Nombre invalido, no olvide las comillas")
                            )
                            (setf (Customer-name tempCustomer) tempName)

                            (loop
                                (print "Digite su apellido (con comillas, ejemplo: \"Perez\"): ")
                                (setq tempLastName (read))
                                (when (stringp tempLastName) (return))
                                (print "Apellido invalido, no olvide las comillas")
                            )
                            (setf (Customer-lastName tempCustomer) tempLastName)

                            (loop
                                (print "Digite su direccion (con comillas, ejemplo: \"Cra 1 28N11\"): ")
                                (setq tempAddress (read))
                                (when (stringp tempAddress) (return))
                                (print "Direccion invalida, no olvide las comillas")
                            )
                            (setf (Customer-address tempCustomer) tempAddress)

                            (loop
                                (print "Digite su numero de telefono (numero entero positivo): ")
                                (setq tempPhone (read))
                                (when (and (integerp tempPhone) (> tempPhone 0)) (return))
                                (print "Telefono invalido, debe ser un numero entero positivo")
                            )
                            (setf (Customer-phoneNumber tempCustomer) tempPhone)
                            (setf (Purchase-customer purchase) tempCustomer)

                            ; seleccionar libro usando funcion auxiliar
                            (setf (Purchase-book purchase) (selectBook))

                            ; pedir fecha
                            (setf (Purchase-date purchase) (askDate))

                            ; asignar compra y guardar
                            (setf (aref purchaseArray purchaseCount) purchase)
                            (setq purchaseCount (+ purchaseCount 1))

                            ; mostrar resumen sin descuento
                            (format t "~%--- RESUMEN DE COMPRA ---~%")
                            (format t "Libro            : ~S~%" (Book-title (Purchase-book purchase)))
                            (format t "Fecha            : ~S~%" (Purchase-date purchase))
                            (format t "Total a pagar    : $~S~%" (Book-price (Purchase-book purchase)))
                            (format t "~%Compra realizada exitosamente!~%")
                        )
                    )
                )
            )
        )
    )
)

; Funcion encargada de mostrar todas las compras registradas
(defun showAllPurchases()
    (if (= purchaseCount 0)
        (print "No hay compras registradas")
        (progn
            (setq i 0)
            (loop
                (setq purchase (aref purchaseArray i))
                (format t "~%--- COMPRA ~S ---~%" (+ i 1))
                (format t "Fecha    : ~S~%" (Purchase-date purchase))
                (format t "Cliente  : ~S ~S~%"
                    (Customer-name (Purchase-customer purchase))
                    (Customer-lastName (Purchase-customer purchase))
                )
                (format t "Libro    : ~S~%" (Book-title (Purchase-book purchase)))
                (setq bookPrice (Book-price (Purchase-book purchase)))
                (if (null (Customer-customerNumber (Purchase-customer purchase)))
                    ; cliente no registrado: precio original
                    (format t "Total pagado     : $~S~%" bookPrice)
                    ; cliente registrado: precio con descuento
                    (progn
                        (setq finalPrice (- bookPrice (* bookPrice 0.1)))
                        (format t "Precio original  : $~S~%" bookPrice)
                        (format t "Descuento (10%%) : $~S~%" (* bookPrice 0.1))
                        (format t "Total pagado     : $~S~%" finalPrice)
                    )
                )
                (setq i (+ i 1))
                (when (>= i purchaseCount) (return))
            )
        )
    )
)

; Funcion encargada de mostrar las compras de un cliente registrado
(defun showCustomerPurchases()
    (if (= purchaseCount 0)
        (print "No hay compras registradas")
        (progn
            (loop
                (print "Digite el numero de cliente (numero entero positivo): ")
                (setq customerNum (read))
                (when (and (integerp customerNum) (> customerNum 0)) (return))
                (print "Numero de cliente invalido, debe ser un numero entero positivo")
            )
            (setq foundCustomer (findCustomerByNumber customerNum))
            (if (null foundCustomer)
                (format t "~%Cliente con numero ~S no encontrado~%" customerNum)
                (progn
                    (setq encontro nil)
                    (setq i 0)
                    (loop
                        (setq purchase (aref purchaseArray i))
                        (setq purchaseCustomer (Purchase-customer purchase))
                        (when (and
                                (not (null (Customer-customerNumber purchaseCustomer)))
                                (= customerNum (Customer-customerNumber purchaseCustomer)))
                            (format t "~%--- COMPRA ~S ---~%" (+ i 1))
                            (format t "Fecha    : ~S~%" (Purchase-date purchase))
                            (format t "Libro    : ~S~%" (Book-title (Purchase-book purchase)))
                            (setq bookPrice (Book-price (Purchase-book purchase)))
                            (setq finalPrice (- bookPrice (* bookPrice 0.1)))
                            (format t "Precio original  : $~S~%" bookPrice)
                            (format t "Descuento (10%%) : $~S~%" (* bookPrice 0.1))
                            (format t "Total pagado     : $~S~%" finalPrice)
                            (setq encontro t)
                        )
                        (setq i (+ i 1))
                        (when (>= i purchaseCount) (return))
                    )
                    (when (null encontro)
                        (format t "~%El cliente ~S ~S no tiene compras registradas~%"
                            (Customer-name foundCustomer)
                            (Customer-lastName foundCustomer)
                        )
                    )
                )
            )
        )
    )
)