#|
  integrantes:
  Nicolas Rocha Gutierrez
  Jose David Hurtado Arandano
|#

; Funcion auxiliar que retorna la fecha actual del sistema
; retorna la fecha como string en formato DD-MM-AAAA
(defun askDate()
    (multiple-value-bind (sec min hour day month year)
        (decode-universal-time (get-universal-time))
        (format nil "~2,'0d-~2,'0d-~d" day month year))
    #|
    (loop
        (format t "Digite el dia de la compra (1-31): ")
        (setq day (prompt-read))
        (when (and (integerp day) (>= day 1) (<= day 31)) (return))
        (format t "Dia invalido, debe ser un numero entre 1 y 31~%")
    )
    (loop
        (format t "Digite el mes de la compra (1-12): ")
        (setq month (prompt-read))
        (when (and (integerp month) (>= month 1) (<= month 12)) (return))
        (format t "Mes invalido, debe ser un numero entre 1 y 12~%")
    )
    (loop
        (format t "Digite el anio de la compra (numero entero positivo): ")
        (setq year (prompt-read))
        (when (and (integerp year) (> year 0)) (return))
        (format t "Anio invalido, debe ser un numero entero positivo~%")
    )
    (format nil "~A/~A/~A" day month year)
    |#
)

; Funcion encargada de registrar la compra de un libro con su respectivo pago
(defun buyBook()
    (if (= bookCount 0)
        (format t "No hay libros disponibles~%")
        (progn
            (if (>= purchaseCount (length purchaseArray))
                (format t "No hay espacio para mas compras~%")
                (progn
                    (setq purchase (make-Purchase))

                    ; preguntar si es cliente registrado
                    (loop
                        (format t "Es usted un cliente registrado?~%")
                        (format t "  1.  Si~%")
                        (format t "  2.  No~%")
                        (format t "~%")
                        (format t "Digite su respuesta (debe digitar el numero): ")
                        (setq isRegistered (prompt-read))
                        (when (and (integerp isRegistered) (or (= isRegistered 1) (= isRegistered 2))) (return))
                        (format t "Opcion invalida, intente de nuevo~%")
                    )

                    (if (= isRegistered 1)
                        ; cliente registrado: buscar por numero de cliente
                        (progn
                            (loop
                                (format t "Digite su numero de cliente (numero entero positivo): ")
                                (setq customerNum (prompt-read))
                                (when (and (integerp customerNum) (> customerNum 0)) (return))
                                (format t "Numero de cliente invalido, debe ser un numero entero positivo~%")
                            )
                            (setq foundCustomer (findCustomerByNumber customerNum))
                            (if (null foundCustomer)
                                (format t "~%Cliente con numero ~A no encontrado~%" customerNum)
                                (progn
                                    (setf (Purchase-customer purchase) foundCustomer)
                                    (format t "~%Bienvenido ~A ~A~%"
                                        (Customer-name foundCustomer)
                                        (Customer-lastName foundCustomer)
                                    )
                                    ; seleccionar libro usando funcion auxiliar
                                    (setf (Purchase-book purchase) (selectBook))

                                    ; tomar fecha
                                    (setf (Purchase-date purchase) (askDate))

                                    ; asignar compra y guardar
                                    (setf (aref purchaseArray purchaseCount) purchase)
                                    (setq purchaseCount (+ purchaseCount 1))
                                    (removeBookFromStock (Book-title (Purchase-book purchase)))

                                    ; calcular precio con descuento
                                    (setq bookPrice (Book-price (Purchase-book purchase)))
                                    (setq discount (* bookPrice discountRate))
                                    (setq finalPrice (- bookPrice discount))

                                    ; mostrar resumen de la compra
                                    (format t "~%--- RESUMEN DE COMPRA ---~%")
                                    (format t "Libro            : ~A~%" (Book-title (Purchase-book purchase)))
                                    (format t "Fecha            : ~A~%" (Purchase-date purchase))
                                    (format t "Precio original  : $~A~%" bookPrice)
                                    (format t "Descuento (~A%) : $~A~%" (* discountRate 100) discount)
                                    (format t "Total a pagar    : $~A~%" finalPrice)
                                    (format t "~%Compra realizada exitosamente!~%")
                                )
                            )
                        )
                        ; cliente no registrado: pedir datos
                        (progn
                            (setq tempCustomer (make-Customer :customerNumber nil))

                            (loop
                                (format t "Digite su nombre (con comillas, ejemplo: \"Juan\"): ")
                                (setq tempName (prompt-read))
                                (when (stringp tempName) (return))
                                (format t "Nombre invalido, no olvide las comillas~%")
                            )
                            (setf (Customer-name tempCustomer) tempName)

                            (loop
                                (format t "Digite su apellido (con comillas, ejemplo: \"Perez\"): ")
                                (setq tempLastName (prompt-read))
                                (when (stringp tempLastName) (return))
                                (format t "Apellido invalido, no olvide las comillas~%")
                            )
                            (setf (Customer-lastName tempCustomer) tempLastName)

                            (loop
                                (format t "Digite su direccion (con comillas, ejemplo: \"Cra 1 28N11\"): ")
                                (setq tempAddress (prompt-read))
                                (when (stringp tempAddress) (return))
                                (format t "Direccion invalida, no olvide las comillas~%")
                            )
                            (setf (Customer-address tempCustomer) tempAddress)

                            (loop
                                (format t "Digite su numero de telefono (numero entero positivo): ")
                                (setq tempPhone (prompt-read))
                                (when (and (integerp tempPhone) (> tempPhone 0)) (return))
                                (format t "Telefono invalido, debe ser un numero entero positivo~%")
                            )
                            (setf (Customer-phoneNumber tempCustomer) tempPhone)
                            (setf (Purchase-customer purchase) tempCustomer)

                            ; seleccionar libro usando funcion auxiliar
                            (setf (Purchase-book purchase) (selectBook))

                            ; tomar fecha
                            (setf (Purchase-date purchase) (askDate))

                            ; asignar compra y guardar
                            (setf (aref purchaseArray purchaseCount) purchase)
                            (setq purchaseCount (+ purchaseCount 1))
                            (removeBookFromStock (Book-title (Purchase-book purchase)))

                            ; mostrar resumen sin descuento
                            (format t "~%--- RESUMEN DE COMPRA ---~%")
                            (format t "Libro            : ~A~%" (Book-title (Purchase-book purchase)))
                            (format t "Fecha            : ~A~%" (Purchase-date purchase))
                            (format t "Total a pagar    : $~A~%" (Book-price (Purchase-book purchase)))
                            (format t "~%Compra realizada exitosamente!~%")
                        )
                    )
                )
            )
        )
    )
)

; Funcion encargada de mostrar todas las compras registradas con Libros
(defun showBookPurchases()
    (if (= purchaseCount 0)
        (format t "No hay compras registradas~%")
        (progn
            (setq i 0)
            (setq encontro nil)
            (loop
                (setq purchase (aref purchaseArray i))
                (when (not (null (Purchase-book purchase)))
                    (setq encontro t)
                    (format t "~%--- COMPRA ~A ---~%" (+ i 1))
                    (format t "Fecha    : ~A~%" (Purchase-date purchase))
                    (format t "Cliente  : ~A ~A~%"
                        (Customer-name (Purchase-customer purchase))
                        (Customer-lastName (Purchase-customer purchase))
                    )
                    (format t "Libro    : ~A~%" (Book-title (Purchase-book purchase)))
                    (setq bookPrice (Book-price (Purchase-book purchase)))
                    (if (null (Customer-customerNumber (Purchase-customer purchase)))
                        ; cliente no registrado: precio original
                        (format t "Total pagado     : $~A~%" bookPrice)
                        ; cliente registrado: precio con descuento
                        (progn
                            (setq finalPrice (- bookPrice (* bookPrice discountRate)))
                            (format t "Precio original  : $~A~%" bookPrice)
                            (format t "Descuento (~A%) : $~A~%" (* discountRate 100) (* bookPrice discountRate))
                            (format t "Total pagado     : $~A~%" finalPrice)
                        )
                    )
                )
                (setq i (+ i 1))
                (when (>= i purchaseCount) (return))
            )
            (when (null encontro)
                (format t "No hay compras de libros registradas~%")
            )
        )
    )
)

; Funcion encargada de mostrar todas las compras registradas con Game
(defun showGamePurchases()
    (if (= purchaseCount 0)
        (format t "No hay compras registradas~%")
        (progn
            (setq i 0)
            (loop
                (setq purchase (aref purchaseArray i))
                (format t "~%--- COMPRA ~A ---~%" (+ i 1))
                (format t "Fecha    : ~A~%" (Purchase-date purchase))
                (format t "Cliente  : ~A ~A~%"
                    (Customer-name (Purchase-customer purchase))
                    (Customer-lastName (Purchase-customer purchase))
                )
                (format t "Juego    : ~A~%" (Game-title (Purchase-game purchase)))
                (setq gamePrice (Game-price (Purchase-game purchase)))
                (if (null (Customer-customerNumber (Purchase-customer purchase)))
                    ; cliente no registrado: precio original
                    (format t "Total pagado     : $~A~%" gamePrice)
                    ; cliente registrado: precio con descuento
                    (progn
                        (setq finalPrice (- gamePrice (* gamePrice discountRate)))
                        (format t "Precio original  : $~A~%" gamePrice)
                        (format t "Descuento (~A%) : $~A~%" (* discountRate 100) (* gamePrice discountRate))
                        (format t "Total pagado     : $~A~%" finalPrice)
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
        (format t "No hay compras registradas~%")
        (progn
            (loop
                (format t "Digite el numero de cliente (numero entero positivo): ")
                (setq customerNum (prompt-read))
                (when (and (integerp customerNum) (> customerNum 0)) (return))
                (format t "Numero de cliente invalido, debe ser un numero entero positivo~%")
            )
            (setq foundCustomer (findCustomerByNumber customerNum))
            (if (null foundCustomer)
                (format t "~%Cliente con numero ~A no encontrado~%" customerNum)
                (progn
                    (setq encontro nil)
                    (setq i 0)
                    (loop
                        (setq purchase (aref purchaseArray i))
                        (setq purchaseCustomer (Purchase-customer purchase))
                        (when (and
                                (not (null (Customer-customerNumber purchaseCustomer)))
                                (= customerNum (Customer-customerNumber purchaseCustomer)))
                            (format t "~%--- COMPRA ~A ---~%" (+ i 1))
                            (format t "Fecha    : ~A~%" (Purchase-date purchase))
                            (format t "Libro    : ~A~%" (Book-title (Purchase-book purchase)))
                            (setq bookPrice (Book-price (Purchase-book purchase)))
                            (setq finalPrice (- bookPrice (* bookPrice discountRate)))
                            (format t "Precio original  : $~A~%" bookPrice)
                            (format t "Descuento (~A%) : $~A~%" (* discountRate 100) (* bookPrice discountRate))
                            (format t "Total pagado     : $~A~%" finalPrice)
                            (setq encontro t)
                        )
                        (setq i (+ i 1))
                        (when (>= i purchaseCount) (return))
                    )
                    (when (null encontro)
                        (format t "~%El cliente ~A ~A no tiene compras registradas~%"
                            (Customer-name foundCustomer)
                            (Customer-lastName foundCustomer)
                        )
                    )
                )
            )
        )
    )
)

;Funcion para comprar jueguitos
(defun buyGame()
    (if (= gameCount 0)
        (format t "No hay juegos disponibles~%")
        (progn
            (if (>= purchaseCount (length purchaseArray))
                (format t "No hay espacio para mas compras~%")
                (progn
                    (setq purchase (make-Purchase))

                    ; preguntar si es cliente registrado
                    (loop
                        (format t "Es usted un cliente registrado?~%")
                        (format t "  1.  Si~%")
                        (format t "  2.  No~%")
                        (format t "~%")
                        (format t "Digite su respuesta (debe digitar el numero): ")
                        (setq isRegistered (prompt-read))
                        (when (and (integerp isRegistered) (or (= isRegistered 1) (= isRegistered 2))) (return))
                        (format t "Opcion invalida, intente de nuevo~%")
                    )

                    (if (= isRegistered 1)
                        ; cliente registrado: buscar por numero de cliente
                        (progn
                            (loop
                                (format t "Digite su numero de cliente (numero entero positivo): ")
                                (setq customerNum (prompt-read))
                                (when (and (integerp customerNum) (> customerNum 0)) (return))
                                (format t "Numero de cliente invalido, debe ser un numero entero positivo~%")
                            )
                            (setq foundCustomer (findCustomerByNumber customerNum))
                            (if (null foundCustomer)
                                (format t "~%Cliente con numero ~A no encontrado~%" customerNum)
                                (progn
                                    (setf (Purchase-customer purchase) foundCustomer)
                                    (format t "~%Bienvenido ~A ~A~%"
                                        (Customer-name foundCustomer)
                                        (Customer-lastName foundCustomer)
                                    )
                                    ; seleccionar juego usando funcion auxiliar
                                    (setf (Purchase-game purchase) (selectGame))

                                    ; tomar fecha
                                    (setf (Purchase-date purchase) (askDate))

                                    ; asignar compra y guardar
                                    (setf (aref purchaseArray purchaseCount) purchase)
                                    (setq purchaseCount (+ purchaseCount 1))
                                    (removeGameFromStock (Game-title (Purchase-game purchase)))

                                    ; calcular precio con descuento
                                    (setq gamePrice (Game-price (Purchase-game purchase)))
                                    (setq discount (* gamePrice discountRate))
                                    (setq finalPrice (- gamePrice discount))

                                    ;llamar a la funcion para el formato del juego
                                    (setq isFormat (selectFormat))


                                    ; mostrar resumen de la compra
                                    (format t "~%--- RESUMEN DE COMPRA ---~%")
                                    (format t "Juego            : ~A~%" (Game-title (Purchase-game purchase)))
                                    (if (= isFormat 1)
                                        (format t "Formato            : Fisico")   
                                        (format t "Formato            : Digital")   
                                    )
                                    (format t "Fecha            : ~A~%" (Purchase-date purchase))
                                    (format t "Precio original  : $~A~%" gamePrice)
                                    (format t "Descuento (~A%) : $~A~%" (* discountRate 100) discount)
                                    (format t "Total a pagar    : $~A~%" finalPrice)
                                    (format t "~%Compra realizada exitosamente!~%")
                                )
                            )
                        )
                        ; cliente no registrado: pedir datos
                        (progn
                            (setq tempCustomer (make-Customer :customerNumber nil))

                            (loop
                                (format t "Digite su nombre (con comillas, ejemplo: \"Juan\"): ")
                                (setq tempName (prompt-read))
                                (when (stringp tempName) (return))
                                (format t "Nombre invalido, no olvide las comillas~%")
                            )
                            (setf (Customer-name tempCustomer) tempName)

                            (loop
                                (format t "Digite su apellido (con comillas, ejemplo: \"Perez\"): ")
                                (setq tempLastName (prompt-read))
                                (when (stringp tempLastName) (return))
                                (format t "Apellido invalido, no olvide las comillas~%")
                            )
                            (setf (Customer-lastName tempCustomer) tempLastName)

                            (loop
                                (format t "Digite su direccion (con comillas, ejemplo: \"Cra 1 28N11\"): ")
                                (setq tempAddress (prompt-read))
                                (when (stringp tempAddress) (return))
                                (format t "Direccion invalida, no olvide las comillas~%")
                            )
                            (setf (Customer-address tempCustomer) tempAddress)

                            (loop
                                (format t "Digite su numero de telefono (numero entero positivo): ")
                                (setq tempPhone (prompt-read))
                                (when (and (integerp tempPhone) (> tempPhone 0)) (return))
                                (format t "Telefono invalido, debe ser un numero entero positivo~%")
                            )
                            (setf (Customer-phoneNumber tempCustomer) tempPhone)
                            (setf (Purchase-customer purchase) tempCustomer)

                            ; seleccionar juego usando funcion auxiliar
                            (setf (Purchase-game purchase) (selectGame))

                            ; tomar fecha
                            (setf (Purchase-date purchase) (askDate))

                            ; asignar compra y guardar
                            (setf (aref purchaseArray purchaseCount) purchase)
                            (setq purchaseCount (+ purchaseCount 1))
                            (removeGameFromStock (Game-title (Purchase-game purchase)))

                            ;llamar a la funcion para el formato del juego
                            (setq isFormat (selectFormat))

                            ; mostrar resumen de la compra
                            (format t "~%--- RESUMEN DE COMPRA ---~%")
                            (format t "Juego            : ~A~%" (Game-title (Purchase-game purchase)))
                            (if (= isFormat 1)
                                (format t "Formato            : Fisico")   
                                (format t "Formato            : Digital")   
                            )
                            (format t "Fecha            : ~A~%" (Purchase-date purchase))
                            (format t "Total a pagar    : $~A~%" finalPrice)
                            (format t "~%Compra realizada exitosamente!~%")
                        )
                    )
                )
            )
        )
    )
)

;Funcion para formato juego
(defun selectFormat()

    ; preguntar formato juego
        
    (loop
        (format t "Selecciene el formato del juego~%")
        (format t "  1.  Fisico~%")
        (format t "  2.  Digital~%")
        (format t "~%")
        (format t "Digite su respuesta (debe digitar el numero): ")
        (setq isFormat (prompt-read))
        (when (and (integerp isFormat) (or (= isFormat 1) (= isFormat 2))) (return))
        (format t "Opcion invalida, intente de nuevo~%")
    )
    (if (= isFormat 1)
        (format t "Juego entregado fisicamente.~%")
        (loop
            (format t "Digite un correo para enviar el juego (con comillas, ejemplo: \"jj@gmai.com\"):~%")
            (setq isMail (prompt-read))
            (when (stringp isMail)
                (format t "Enviado a.  ~A~%" isMail)
                (return)
            )
            (format t "Mail invalido, no olvide las comillas~%")
        )
    )
    isFormat
)