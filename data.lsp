#| 
  integrantes:
  Nicolas Rocha Gutierrez
  Jose David Hurtado Arandano
|#

(defun loadData()
    (setq bookCount 4)
    (defstruct Book
        title
        topic
        author
        editorial
        price
    )

    (defstruct User
        name
        lastName
        address
        phoneNumber
        customerNumber
    )

    (defstruct Purchase
        date
        user
        book
    )

    ;suponiendo que definimos un vector con 10 posiciones (sujeto a cambios)
    (setq bookArray(make-array 10))

    ;inicialice 4 libros 
    (setq book1 (make-Book 
        :title "1000 anios de seriedad"
        :topic "Realismo Magico"
        :author "Gabriel Garcia Marquez"
        :editorial "El pais"
        :price 200000))
    (setf (aref bookArray 0) book1)

    (setq book2 (make-Book 
        :title "El seriesito"
        :topic "Fantasia"
        :author "Antoine de Saint-Exupery"
        :editorial "El mundo"
        :price 300000))
    (setf (aref bookArray 1) book2)

    (setq book3 (make-Book 
        :title "Harry Potter Y el serio de Azkaban"
        :topic "Fantasia"
        :author "J. K. Rowling"
        :editorial "El planeta"
        :price 400000))
    (setf (aref bookArray 2) book3)

    (setq book4 (make-Book 
        :title "Seriostein"
        :topic "Fantasia"
        :author "Mary Shelley"
        :editorial "El pais"
        :price 500000))
    (setf (aref bookArray 3) book4)

)
