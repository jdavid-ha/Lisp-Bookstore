#|
  integrantes:
  Nicolas Rocha Gutierrez
  Jose David Hurtado Arandano
|#

; Funcion auxiliar que muestra el menu de temas y retorna el tema seleccionado
(defun selectTopic()
    (loop
        (format t "Seleccione el tema:~%")
        (format t "  1.  Fantasia~%")
        (format t "  2.  Romance~%")
        (format t "  3.  Ciencia Ficcion~%")
        (format t "  4.  Terror~%")
        (format t "  5.  Realismo Magico~%")
        (format t "~%")
        (format t "Digite su eleccion (debe digitar el numero, no el nombre del tema): ")
        (setq opcionTema (prompt-read))
        (when (and (integerp opcionTema) (>= opcionTema 1) (<= opcionTema 5)) (return))
        (format t "Opcion invalida, intente de nuevo~%")
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
        (format t "No hay espacio para mas libros~%")
        (progn
            (setq book (make-Book))
            ;valida nombre del libro
            (loop
                (format t "Digite el nombre del libro (con comillas, ejemplo: \"El Principito\"): ")
                (setq bookTitle (prompt-read))
                (when (stringp bookTitle) (return))
                (format t "Nombre del libro no aceptado, no olvide las comillas~%")
            )
            (setf (Book-title book) bookTitle)

            ;Aqui uso la funcon auxiliar para el tema
            (setf (Book-topic book) (selectTopic))

            ;valida nombre del autor
            (loop
                (format t "Digite el nombre del autor (con comillas, ejemplo: \"Antoine de Saint-Exupery\"): ")
                (setq bookAuthor (prompt-read))
                (when (stringp bookAuthor) (return))
                (format t "Nombre del autor no aceptado, no olvide las comillas~%")
            )
            (setf (Book-author book) bookAuthor)

            ;valida nombre de la editorial
            (loop
                (format t "Digite el nombre de la editorial (con comillas, ejemplo: \"Reynal & Hitchcock\"): ")
                (setq  bookEditorial (prompt-read))
                (when (stringp bookEditorial) (return))
                (format t "Nombre de la editorial no aceptado, no olvide las comillas~%")
            )
            (setf (Book-editorial book) bookEditorial)

            ;valida precio del libro
            (loop
                (format t "Digite el precio del libro (numero entero positivo): ")
                (setq bookPrice (prompt-read))
                (when (and (integerp bookPrice) (> bookPrice 0)) (return))
                (format t "Precio invalido, debe ser un numero entero positivo~%")
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
        (format t "No hay libros registrados~%")
        (progn
            (setq topic (selectTopic))
            (setq i 0)
            (setq encontro nil)
            (loop
                (when (string= topic (Book-topic (aref bookArray i)))
                    (setq book (aref bookArray i))
                    (format t "~%Nombre: ~A~%" (Book-title book))
                    (format t "Tema: ~A~%" (Book-topic book))
                    (format t "Autor: ~A~%" (Book-author book))
                    (format t "Editorial: ~A~%" (Book-editorial book))
                    (format t "Precio: $~A~%" (Book-price book))
                    (setq encontro t)
                )
                (setq i (+ i 1))
                (when (>= i bookCount) (return))
            )
            (when (null encontro)
                (format t "~%No se encontraron libros con el tema ~A~%" topic)
            )
        )
    )
)

; Funcion encargada de mostrar todos los libros
(defun showAllBooks()
    (if (= bookCount 0)
        (format t "No hay libros registrados~%")
        (progn
            (setq i 0)
            (loop
                (setq book (aref bookArray i))
                (format t "~%Nombre: ~A~%" (Book-title book))
                (format t "Tema: ~A~%" (Book-topic book))
                (format t "Autor: ~A~%" (Book-author book))
                (format t "Editorial: ~A~%" (Book-editorial book))
                (format t "Precio: $~A~%" (Book-price book))
                (setq i (+ i 1))
                (when (>= i bookCount)(return))
            )
        )
    )
)

; Funcion encargada de buscar un libro por su nombre
(defun searchSpecificBook ()
    (if (= bookCount 0)
        (format t "No hay libros registrados~%")
        (progn
            (loop
                (format t "Digite el nombre del libro que desea buscar (con comillas, ejemplo: \"El Principito\"): ")
                (setq title (prompt-read))
                (when (stringp title) (return))
                (format t "Nombre del libro no aceptado, no olvide las comillas~%")
            )
            (setq i 0)
            (setq encontro nil)
            (loop
                (when (string= title (Book-title (aref bookArray i)))
                    (setq book (aref bookArray i))
                    (format t "~%Nombre: ~A~%" (Book-title book))
                    (format t "Tema: ~A~%" (Book-topic book))
                    (format t "Autor: ~A~%" (Book-author book))
                    (format t "Editorial: ~A~%" (Book-editorial book))
                    (format t "Precio: $~A~%" (Book-price book))
                    (setq encontro t)
                )
                (setq i (+ i 1))
                (when (>= i bookCount) (return))
            )
            (when (null encontro)
                (format t "~%No se encontro el libro ~A~%" title)
            )
        )
    )
)

; Elimina un libro del stock dado su titulo (llamada internamente tras una compra)
(defun removeBookFromStock(bookTitle)
    ; buscar el libro en el vector
    (setq posFound -1)
    (setq i 0)
    (loop
        (when (string= bookTitle (Book-title (aref bookArray i)))
            (setq posFound i)
        )
        (setq i (+ i 1))
        (when (>= i bookCount) (return))
    )
    ; si se encontro, correr los libros hacia atras y bajar el contador
    (when (>= posFound 0)
        (setq i posFound)
        (loop
            (setf (aref bookArray i) (aref bookArray (+ i 1)))
            (setq i (+ i 1))
            (when (>= i (- bookCount 1)) (return))
        )
        ; limpiar ultima posicion y bajar contador
        (setf (aref bookArray (- bookCount 1)) nil)
        (setq bookCount (- bookCount 1))
    )
)

; Funcion encargada de eliminar un libro del vector por su titulo leído
(defun deleteBook()
    ; validar que haya libros
    (if (= bookCount 0)
        (format t "No hay libros registrados~%")
        (progn
            ; pedir titulo del libro a eliminar
            (loop
                (format t "Digite el nombre del libro a eliminar (con comillas, ejemplo: \"El Principito\"): ")
                (setq titleToDelete (prompt-read))
                (when (stringp titleToDelete) (return))
                (format t "Nombre no aceptado, no olvide las comillas~%")
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
                (format t "~%Libro ~A no encontrado~%" titleToDelete)
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
                    (format t "~%Libro ~A eliminado exitosamente~%" titleToDelete)
                )
            )
        )
    )
)

; Funcion encargada de mostrar los descuentos de los libros solo para clientes registrados
(defun showDiscounts()
    ; validar que haya libros
    (if (= bookCount 0)
        (format t "No hay libros registrados~%")
        (progn
            (format t "~%NOTA: Los precios con descuento son exclusivos para clientes registrados~%")
            (format t "~%--- DESCUENTOS PARA CLIENTES REGISTRADOS ---~%")
            (setq i 0)
            (loop
                (setq book (aref bookArray i))
                (setq bookPrice (Book-price book))
                (setq precioDescuento (- bookPrice (* bookPrice discountRate)))
                (format t "~%Nombre              : ~A~%" (Book-title book))
                (format t "Precio original     : $~A~%" bookPrice)
                (format t "Descuento           : ~A%~%" (* discountRate 100))
                (format t "Precio con descuento: $~A~%" precioDescuento)
                (setq i (+ i 1))
                (when (>= i bookCount) (return))
            )
        )
    )
)

; Funcion auxiliar que muestra los libros disponibles y retorna el libro seleccionado
(defun selectBook()
    (format t "~%--- LIBROS DISPONIBLES ---~%")
    (setq i 0)
    (loop
        (setq book (aref bookArray i))
        (format t "~A. ~A - ~A - $~A~%"
            (+ i 1)
            (Book-title book)
            (Book-topic book)
            (Book-price book)
        )
        (setq i (+ i 1))
        (when (>= i bookCount) (return))
    )
    (loop
        (format t "Seleccione el numero del libro: ")
        (setq bookOption (prompt-read))
        (when (and (integerp bookOption) (>= bookOption 1) (<= bookOption bookCount)) (return))
        (format t "Opcion invalida, intente de nuevo~%")
    )
    (aref bookArray (- bookOption 1))
)
