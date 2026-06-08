#| 
  integrantes:
  Nicolas Rocha Gutierrez
  Jose David Hurtado Arandano
|#

(defun load()
    (defstruct Book
    name
    topic
    author
    editorial
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
)
