#| 
  integrantes:
  Nicolas Rocha Gutierrez
  Jose David Hurtado Arandano
|#

(defun prompt-read ()
    (finish-output)
    (read))

(defun loadData()
    ; variable global que ayuda a contar los libros que hay
    (setq bookCount 4)
    ; variable global que ayuda a contar los clientes que hay
    (setq customerCount 4)
    ; variable global que ayuda a contar las compras realizadas
    (setq purchaseCount 0)
    ; variable que permite colocar el numero de cliente automaticamente
    (setq customerNumberCount 104)
    ; variable global que representa el porcentaje de descuento para clientes registrados
    (setq discountRate 0.1)
    ;Variable que cuenta los juegos
    (setq gameCount 4)
    (defstruct Book
        title
        topic
        author
        editorial
        price
    )

    (defstruct Customer
        name
        lastName
        address
        phoneNumber
        customerNumber
    )

    (defstruct Purchase
        date
        customer
        book
        game
    )

    (defstruct Game
        title
        category
        price
    )

    ;vector de juegos con 10 posiciones
    (setq gameArray (make-array 10))
    (setq game1 (make-Game
        :title "fornike"
        :category "Battle royale"
        :price 100000))
    (setf (aref gameArray 0) game1)

    (setq game2 (make-Game
        :title "Krlos duty"
        :category "Shooter"
        :price 300000))
    (setf (aref gameArray 1) game2)

    (setq game3 (make-Game
        :title "Valito"
        :category "Tatical Shoo"
        :price 5000))
    (setf (aref gameArray 2) game3)

    (setq game4 (make-Game
        :title "it takes two"
        :category "Family Friendly"
        :price 156000))
    (setf (aref gameArray 3) game4)

    ; vector de libros con 10 posiciones
    (setq bookArray (make-array 10))

    ; inicializar 4 libros
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

    ; vector de clientes con 10 posiciones
    (setq customerArray (make-array 10))

    ; inicializar 4 clientes
    (setq customer1 (make-Customer
        :name "Luisa"
        :lastName "Fernadez"
        :address "Por ahi en Silvia"
        :phoneNumber 3117612386
        :customerNumber 100))
    (setf (aref customerArray 0) customer1)

    (setq customer2 (make-Customer
        :name "Fernanda"
        :lastName "Pastrana"
        :address "Por ahi en Popayan"
        :phoneNumber 3217612382
        :customerNumber 101))
    (setf (aref customerArray 1) customer2)

    (setq customer3 (make-Customer
        :name "Nicolas"
        :lastName "Rocha"
        :address "Cra 3ra 20N11"
        :phoneNumber 3016132674
        :customerNumber 102))
    (setf (aref customerArray 2) customer3)

    (setq customer4 (make-Customer
        :name "Jose"
        :lastName "Con tilde"
        :address "Por ahi Cartago"
        :phoneNumber 3676776677
        :customerNumber 103))
    (setf (aref customerArray 3) customer4)

    ; vector de compras con 10 posiciones
    (setq purchaseArray (make-array 10))


)