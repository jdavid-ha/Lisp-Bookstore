#| 
  integrantes:
  Nicolas Rocha Gutierrez
  Jose David Hurtado Arandano
|#

(defun loadData()
    ;variable global que ayuda a contar los libros que hay (Cambiar si se queman mas libros)
    (setq bookCount 4)
    ;variable global que ayuda a contar los clientes que hay (Cambiar si se queman mas clientes)
    (setq userCount 4)
    ;variable que permite colocar el numero de cliente automaticamente, cuando se añada un cliente
    ;esta variable se incrementara en 1
    (setq customerNumberCount 104)

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

    ;suponiendo que definimos un vector de libros con 10 posiciones (sujeto a cambios)
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

    ;suponiendo que definimos un vector de clientes con 10 posiciones (sujeto a cambios)
    (setq userArray(make-array 10))

    ;inicialice 4 usuarios

    (setq user1(make-User
        :name "Luisa"
        :lastName "Fernadez"
        :address "Por ahi en Silvia"
        :phoneNumber 3117612386
        :customerNumber 100))
    (setf (aref userArray 0) user1)

    (setq user2(make-User
        :name "Fernanda"
        :lastName "Pastrana"
        :address "Por ahi en Popayan"
        :phoneNumber 3217612382
        :customerNumber 101))
    (setf (aref userArray 1) user2)

    (setq user3(make-User
        :name "Nicolas"
        :lastName "Rocha"
        :address "Cra 3ra 20N11"
        :phoneNumber 3016132674
        :customerNumber 102))
    (setf (aref userArray 2) user3)

    (setq user4(make-User
        :name "Jose"
        :lastName "Con tilde"
        :address "Por ahi Cartago"
        :phoneNumber 3676776677
        :customerNumber 103))
    (setf (aref userArray 3) user4)

)
