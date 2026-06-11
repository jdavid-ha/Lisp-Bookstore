#|
  integrantes:
  Nicolas Rocha Gutierrez
  Jose David Hurtado Arandano
|#

; Funcion que se encarga de registrar un cliente
(defun registerCustomer()
    (if (>= customerCount (length customerArray))
        (format t "No hay espacio para mas clientes~%")
        (progn
            (setq customer (make-Customer))

            ; valida nombre del cliente
            (loop
                (format t "Digite el nombre del cliente (con comillas, ejemplo: \"Luisa\"): ")
                (setq customerName (prompt-read))
                (when (stringp customerName) (return))
                (format t "Nombre no valido, no olvide las comillas~%")
            )
            (setf (Customer-name customer) customerName)

            ; valida apellido del cliente
            (loop
                (format t "Digite el apellido del cliente (con comillas, ejemplo: \"Fernandez\"): ")
                (setq customerLastName (prompt-read))
                (when (stringp customerLastName) (return))
                (format t "Apellido no valido, no olvide las comillas~%")
            )
            (setf (Customer-lastName customer) customerLastName)

            ; valida direccion del cliente
            (loop
                (format t "Digite la direccion del cliente (con comillas, ejemplo: \"Cra 3ra 28N11\"): ")
                (setq customerAddress (prompt-read))
                (when (stringp customerAddress) (return))
                (format t "Direccion no valida, no olvide las comillas~%")
            )
            (setf (Customer-address customer) customerAddress)

            ; valida numero de celular del cliente
            (loop
                (format t "Digite el numero de celular del cliente (numero entero positivo): ")
                (setq customerPhone (prompt-read))
                (when (and (integerp customerPhone) (> customerPhone 0)) (return))
                (format t "Numero de telefono invalido, debe ser un numero entero positivo~%")
            )
            (setf (Customer-phoneNumber customer) customerPhone)

            ; se coloca de manera automatica el numero de cliente
            (setf (Customer-customerNumber customer) customerNumberCount)
            (setq customerNumberCount (+ 1 customerNumberCount))

            ; se guarda el cliente en el vector
            (setf (aref customerArray customerCount) customer)
            (setq customerCount (+ customerCount 1))
            (format t "~%Cliente registrado exitosamente con numero ~A~%" (Customer-customerNumber customer))
        )
    )
)

; Funcion que se encarga de mostrar los clientes registrados
(defun showRegisteredCustomers()
    (if (= customerCount 0)
        (format t "No hay clientes registrados~%")
        (progn
            (setq i 0)
            (loop
                (setq customer (aref customerArray i))
                (format t "~%Nombre          : ~A~%" (Customer-name customer))
                (format t "Apellido        : ~A~%" (Customer-lastName customer))
                (format t "Direccion       : ~A~%" (Customer-address customer))
                (format t "Numero Telefono : ~A~%" (Customer-phoneNumber customer))
                (format t "Numero Cliente  : ~A~%" (Customer-customerNumber customer))
                (setq i (+ i 1))
                (when (>= i customerCount) (return))
            )
        )
    )
)

; Funcion que se encarga de eliminar un cliente registrado por su numero de cliente
(defun deleteRegisteredCustomers()
    (if (= customerCount 0)
        (format t "No hay clientes registrados~%")
        (progn
            ; pedir numero de cliente a eliminar
            (loop
                (format t "Digite el numero de cliente a eliminar (numero entero positivo): ")
                (setq customerNumberToDelete (prompt-read))
                (when (and (integerp customerNumberToDelete) (> customerNumberToDelete 0)) (return))
                (format t "Numero de cliente no valido, debe ser un numero entero positivo~%")
            )

            ; buscar el cliente en el vector
            (setq posFound -1)
            (setq i 0)
            (loop
                (when (= customerNumberToDelete (Customer-customerNumber (aref customerArray i)))
                    (setq posFound i)
                )
                (setq i (+ i 1))
                (when (>= i customerCount) (return))
            )

            ; si no se encontro mostrar mensaje
            (if (= posFound -1)
                (format t "~%Cliente con numero ~A no encontrado~%" customerNumberToDelete)
                ; si se encontro correr los clientes hacia atras
                (progn
                    (setq i posFound)
                    (loop
                        (setf (aref customerArray i) (aref customerArray (+ i 1)))
                        (setq i (+ i 1))
                        (when (>= i (- customerCount 1)) (return))
                    )
                    ; limpiar ultima posicion y bajar contador
                    (setf (aref customerArray (- customerCount 1)) nil)
                    (setq customerCount (- customerCount 1))
                    (format t "~%Cliente con numero ~A eliminado exitosamente~%" customerNumberToDelete)
                )
            )
        )
    )
)

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
