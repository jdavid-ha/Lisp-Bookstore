#| 
  integrantes:
  Nicolas Rocha Gutierrez
  Jose David Hurtado Arandano
|#

; Funcion auxiliar que muestra el menu de temas y retorna el tema seleccionado
(defun selectTopic()
    (loop
        (print "Seleccione el tema:")
        (print "  1.  Fantasia")
        (print "  2.  Romance")
        (print "  3.  Ciencia Ficcion")
        (print "  4.  Terror")
        (print "  5.  Realismo Magico")
        (format t "~%")
        (print "Digite su eleccion (debe digitar el numero, no el nombre del tema): ")
        (setq opcionTema (read))
        (when (and (integerp opcionTema) (>= opcionTema 1) (<= opcionTema 5)) (return))
        (print "Opcion invalida, intente de nuevo")
    )
    (case opcionTema
        (1 "Fantasia")
        (2 "Romance")
        (3 "Ciencia Ficcion")
        (4 "Terror")
        (5 "Realismo Magico")
    )
)

; Funcion encargada de añadir un libro
(defun addBook()
    (if (>= bookCount (length bookArray))
        (print "No hay espacio para mas libros")
        (progn
            (setq book (make-Book))
            ;valida nombre del libro
            (loop   
                (print "Digite el nombre del libro(con comillas, ejemplo: \"El Principito\"): ")
                (setq bookTitle (read))
                (when (stringp bookTitle) (return))
                (print "Nombre del libro no aceptado, no olvide las comillas")
            )
            (setf (Book-title book) bookTitle)

            ;Aqui uso la funcon auxiliar para el tema 
            (setf (Book-topic book) (selectTopic))

            ;valida nombre del autor
            (loop   
                (print "Digite el nombre del autor(con comillas, ejemplo: \"Antoine de Saint-Exupery\"): ")
                (setq bookAuthor (read))
                (when (stringp bookAuthor) (return))
                (print "Nombre del autor no aceptado, no olvide las comillas")
            )
            (setf (Book-author book) bookAuthor)

            ;valida nombre de la editorial
            (loop   
                (print "Digite el nombre de la editorial(con comillas, ejemplo: \"Reynal & Hitchcock\"): ")
                (setq  bookEditorial (read))
                (when (stringp bookEditorial) (return))
                (print "Nombre de la editorial no aceptado, no olvide las comillas")
            )
            (setf (Book-editorial book) bookEditorial)

            ;valida precio del libro
            (loop
                (print "Digite el precio del libro (numero entero positivo): ")
                (setq bookPrice (read))
                (when (and (integerp bookPrice) (> bookPrice 0)) (return))
                (print "Precio invalido, debe ser un numero entero positivo")
            )
            (setf (Book-price book) bookPrice)
            (setf (aref bookArray bookCount) book)
            (setq bookCount (+ bookCount 1))
        )
    )    
)

; Funcion encargada de buscar libros por su tema
(defun showBooksByTopic ()
    (if (= bookCount 0)
        (print "No hay libros registrados")
        (progn
            (setq topic (selectTopic))
            (setq i 0)
            (setq encontro nil)
            (loop
                (when (string= topic (Book-topic (aref bookArray i)))
                    (setq book (aref bookArray i))
                    (format t "~%Nombre: ~S~%" (Book-title book))
                    (format t "Tema: ~S~%" (Book-topic book))
                    (format t "Autor: ~S~%" (Book-author book))
                    (format t "Editorial: ~S~%" (Book-editorial book))
                    (format t "Precio: $~S~%" (Book-price book))
                    (setq encontro t)
                )
                (setq i (+ i 1))
                (when (>= i bookCount) (return))
            )
            (when (null encontro)
                (format t "~%No se encontraron libros con el tema ~S ~%" topic)
            )
        )
    )
)

; Funcion encargada de mostrar todos los libros 
(defun showAllBooks()
    (if (= bookCount 0)
        (print "No hay libros registrados")
        (progn
            (setq i 0)
            (loop
                (setq book (aref bookArray i))
                (format t "~%Nombre: ~S~%" (Book-title book))
                (format t "Tema: ~S~%" (Book-topic book))
                (format t "Autor: ~S~%" (Book-author book))
                (format t "Editorial: ~S~%" (Book-editorial book))
                (format t "Precio: $~S~%" (Book-price book))
                (setq i (+ i 1))
                (when (>= i bookCount)(return))
            )
        )
    )
)


; Funcion encargada de buscar un libro por su nombre
(defun searchSpecificBook ()
    (if (= bookCount 0)
        (print "No hay libros registrados")
        (progn
            (loop   
                (print "Digite el nombre del libro que desea buscar (con comillas, ejemplo: \"El Principito\"): ")
                (setq title (read))
                (when (stringp title) (return))
                (print "Nombre del libro no aceptado, no olvide las comillas")
            )
            (setq i 0)
            (setq encontro nil)
            (loop
                (when (string= title (Book-title (aref bookArray i)))
                    (setq book (aref bookArray i))
                    (format t "~%Nombre: ~S~%" (Book-title book))
                    (format t "Tema: ~S~%" (Book-topic book))
                    (format t "Autor: ~S~%" (Book-author book))
                    (format t "Editorial: ~S~%" (Book-editorial book))
                    (format t "Precio: $~S~%" (Book-price book))
                    (setq encontro t)
                )
                (setq i (+ i 1))
                (when (>= i bookCount) (return))
            )
            (when (null encontro)
                (format t "~%No se encontraron el libro ~S ~%" title)
            )
        )
    )
)


; Funcion encargada de eliminar un libro del vector por su titulo
(defun deleteBook()
    ; validar que haya libros
    (if (= bookCount 0)
        (print "No hay libros registrados")
        (progn
            ; pedir titulo del libro a eliminar
            (loop
                (print "Digite el nombre del libro a eliminar (con comillas, ejemplo: \"El Principito\"): ")
                (setq titleToDelete (read))
                (when (stringp titleToDelete) (return))
                (print "Nombre no aceptado, no olvide las comillas")
            )

            ; buscar el libro en el vector
            (setq posFound -1)
            (setq i 0)
            (loop
                (when (string= titleToDelete (Book-title (aref bookArray i)))
                    (setq posFound i)
                )
                (setq i (+ i 1))
                (when (>= i bookCount) (return))
            )

            ; si no se encontro mostrar mensaje
            (if (= posFound -1)
                (format t "~%Libro ~S no encontrado~%" titleToDelete)
                ; si se encontro correr los libros hacia atras
                (progn
                    (setq i posFound)
                    (loop
                        (setf (aref bookArray i) (aref bookArray (+ i 1)))
                        (setq i (+ i 1))
                        (when (>= i (- bookCount 1)) (return))
                    )
                    ; limpiar ultima posicion y bajar contador
                    (setf (aref bookArray (- bookCount 1)) nil)
                    (setq bookCount (- bookCount 1))
                    (format t "~%Libro ~S eliminado exitosamente~%" titleToDelete)
                )
            )
        )
    )
)

; Funcion encargada de mostrar los descuentos de los libros solo para clientes registrados
(defun showDiscounts()
    ; validar que haya libros
    (if (= bookCount 0)
        (print "No hay libros registrados")
        (progn
            (format t "~%NOTA: Los precios con descuento son exclusivos para clientes registrados~%")
            (format t "~%--- DESCUENTOS PARA CLIENTES REGISTRADOS ---~%")
            (setq i 0)
            (loop
                (setq book (aref bookArray i))
                (setq bookPrice (Book-price book))
                (setq precioDescuento (- bookPrice (* bookPrice discountRate)))
                (format t "~%Nombre              : ~S~%" (Book-title book))
                (format t "Precio original     : $~S~%" bookPrice)
                (format t "Descuento           : ~S%~%" (* discountRate 100))
                (format t "Precio con descuento: $~S~%" precioDescuento)
                (setq i (+ i 1))
                (when (>= i bookCount) (return))
            )
        )
    )
)