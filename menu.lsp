#| 
  integrantes:
  Nicolas Rocha Gutierrez
  Jose David Hurtado Arandano
|#

(load "data.lsp")
(load "books.lsp")
(load "customers.lsp")
(load "purchases.lsp")

(loadData)

(print "NOTA: Los datos de tipo texto deben ingresarse CON comillas. Ejemplo: \"Juan\"")
(format t "~%")

; Menu principal
(loop
    (print " ===========================================")
    (print " |                M E N U                  |")
    (print " ===========================================")
    (format t "~%")
    (print "   1.  Libros")
    (print "   2.  Clientes")
    (print "   3.  Compras")
    (print "   4.  Salir")
    (format t "~%")
    (print "Digite su opcion: ")
    (setq option (read))
    (case option
        ; mini menu libros
        (1
            (loop
                (print " ===========================================")
                (print " |              L I B R O S                |")
                (print " ===========================================")
                (format t "~%")
                (print "   1.  Agregar libro")
                (print "   2.  Eliminar libro")
                (print "   3.  Listar libros por tema")
                (print "   4.  Mostrar todos los libros")
                (print "   5.  Buscar un libro especifico")
                (print "   6.  Mostrar descuentos en libros")
                (print "   7.  Volver al menu principal")
                (format t "~%")
                (print "Digite su opcion: ")
                (setq optionBooks (read))
                (case optionBooks
                    (1 (addBook))
                    (2 (deleteBook))
                    (3 (showBooksByTopic))
                    (4 (showAllBooks))
                    (5 (searchSpecificBook))
                    (6 (showDiscounts))
                    (7 (return))
                    (otherwise (print "Opcion invalida, intente de nuevo"))
                )
            )
        )
        ; mini menu clientes
        (2
            (loop
                (print " ===========================================")
                (print " |            C L I E N T E S              |")
                (print " ===========================================")
                (format t "~%")
                (print "   1.  Registrar cliente")
                (print "   2.  Mostrar clientes registrados")
                (print "   3.  Eliminar cliente registrado")
                (print "   4.  Volver al menu principal")
                (format t "~%")
                (print "Digite su opcion: ")
                (setq optionCustomers (read))
                (case optionCustomers
                    (1 (registerCustomer))
                    (2 (showRegisteredCustomers))
                    (3 (deleteRegisteredCustomers))
                    (4 (return))
                    (otherwise (print "Opcion invalida, intente de nuevo"))
                )
            )
        )
        ; mini menu compras
        (3
            (loop
                (print " ===========================================")
                (print " |             C O M P R A S               |")
                (print " ===========================================")
                (format t "~%")
                (print "   1.  Comprar libro")
                (print "   2.  Mostrar todas las compras")
                (print "   3.  Mostrar compras de un cliente registrado")
                (print "   4.  Volver al menu principal")
                (format t "~%")
                (print "Digite su opcion: ")
                (setq optionPurchases (read))
                (case optionPurchases
                    (1 (buyBook))
                    (2 (showAllPurchases))
                    (3 (showCustomerPurchases))
                    (4 (return))
                    (otherwise (print "Opcion invalida, intente de nuevo"))
                )
            )
        )
        (4 (print "Fin del programa") (return))
        (otherwise (print "Opcion invalida, intente de nuevo"))
    )
)