#| 
  integrantes:
  Nicolas Rocha Gutierrez
  Jose David Hurtado Arandano
|#

;Funcion que se encarga de registrar un cliente 
(defun registerCustomer()
    (if (>= userCount (length userArray))
        (print "No hay espacio para mas usuarios")
        (progn
            (setq user (make-User))
            ;valida nombre del cliente
            (loop   
                (print "Digite el nombre del cliente(con comillas, ejemplo: \"Luisa\"): ")
                (setq userName (read))
                (when (stringp userName) (return))
                (print "Nombre no valido, no olvide las comillas")
            )
            (setf (User-name user) userName)

            ;valida apellido del cliente
            (loop   
                (print "Digite el apellido del cliente(con comillas, ejemplo: \"Fernanda\"): ")
                (setq userLastName (read))
                (when (stringp userLastName) (return))
                (print "Apellido no valido, no olvide las comillas")
            )
            (setf (User-lastName user) userLastName)

            ;valida direccion del cliente
            (loop   
                (print "Digite la direccion del cliente (con comillas, ejemplo: \"Cra 3ra 28N11\"): ")
                (setq userAddress (read))
                (when (stringp userAddress) (return))
                (print "Direccion no valida, no olvide las comillas")
            )
            (setf (User-address user) userAddress)

            ;valida numero de celular del cliente
            (loop
                (print "Digite el numero de celular del cliente(numero entero positivo): ")
                (setq userPhoneNumber (read))
                (when (and (integerp userPhoneNumber) (> userPhoneNumber 0)) (return))
                (print "Numero de telefono invalido, debe ser un numero entero positivo")
            )
            (setf (User-phoneNumber user) userPhoneNumber)

            ;se coloca de manera automatica el numero de cliente (customerNumber)
            (setf (User-customerNumber user) customerNumberCount)
            (setq customerNumberCount (+ 1 customerNumberCount))

            ;se guarda el cliente en el vector
            (setf (aref userArray userCount) user)
            (setq userCount (+ userCount 1))
        )
    )
)

;Funcion que se encarga de mostrar los clientes registrados 
(defun showRegisteredCustomers()
    (if (= userCount 0)
        (print "No hay clientes registrados")
        (progn
            (setq i 0)
            (loop
                (setq user (aref userArray i))
                (format t "~%Nombre: ~S~%" (User-name user))
                (format t "Apellido: ~S~%" (User-lastName user))
                (format t "Direccion: ~S~%" (User-address user))
                (format t "Numero de Telefono: ~S~%" (User-phoneNumber user))
                ;Posiblemente no sea util
                (when (/= 0 (User-customerNumber user))
                    (format t "Numero de cliente: $~S~%" (User-customerNumber user))
                )
                (setq i (+ i 1))
                (when (>= i userCount)(return))
            )
        )
    )
)

;Funcion que se encarga de eliminar un cliente registrado
(defun deleteRegisteredCustomers()
    (if(= userCount 0)
        (print "No hay clientes registrados")
        (progn
            ; pedir numero de cliente a eliminar
            (loop
                (print "Digite el numero de cliente a eliminar (numero entero positivo): ")
                (setq customerNumberToDelete (read))
                (when (and (integerp customerNumberToDelete) (> customerNumberToDelete 0)) (return))
                (print "Numero de cliente no valido , debe ser un numero entero positivo")
            )
            ;buscar el cliente en el vector
            (setq posFound -1)
            (setq i 0)
            (loop
                (when (= customerNumberToDelete (User-customerNumber (aref userArray i)))
                    (setq posFound i)
                )
                (setq i (+ i 1))
                (when (>= i userCount) (return))
            )

            ; si no se encontro mostrar mensaje
            (if (= posFound -1)
                (format t "~%Cliente con numero de cliente ~S no encontrado~%" customerNumberToDelete)
                ; si se encontro correr los clientes hacia atras
                (progn
                    (setq i posFound)
                    (loop
                        (setf (aref userArray i) (aref userArray (+ i 1)))
                        (setq i (+ i 1))
                        (when (>= i (- userCount 1)) (return))
                    )
                    ; limpiar ultima posicion y bajar contador
                    (setf (aref userArray (- userCount 1)) nil)
                    (setq userCount (- userCount 1))
                    (format t "~%Cliente con numero de cliente ~S eliminado exitosamente~%" customerNumberToDelete)
                )
            )
        )
    )
)

