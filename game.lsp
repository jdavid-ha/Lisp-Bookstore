#|
  integrantes:
  Nicolas Rocha Gutierrez
  Jose David Hurtado Arandano
|#

; Funcion auxiliar que muestra los juegos disponibles y retorna el juego seleccionado
(defun selectGame()
    (format t "~%--- JUEGOS DISPONIBLES ---~%")
    (setq i 0)
    (loop
        (setq game (aref gameArray i))
        (format t "~A. ~A - ~A - $~A~%"
            (+ i 1)
            (Game-title game)
            (Game-category game)
            (Game-price game)
        )
        (setq i (+ i 1))
        (when (>= i gameCount) (return))
    )
    (loop
        (format t "Seleccione el numero del juego: ")
        (setq gameOption (prompt-read))
        (when (and (integerp gameOption) (>= gameOption 1) (<= gameOption gameCount)) (return))
        (format t "Opcion invalida, intente de nuevo~%")
    )
    (aref gameArray (- gameOption 1))
)

; Elimina un juego del stock dado su titulo (llamada internamente tras una compra)
(defun removeGameFromStock(gameTitle)
    ; buscar el juego en el vector
    (setq posFound -1)
    (setq i 0)
    (loop
        (when (string= gameTitle (Game-title (aref gameArray i)))
            (setq posFound i)
        )
        (setq i (+ i 1))
        (when (>= i gameCount) (return))
    )
    ; si se encontro, correr los juegos hacia atras y bajar el contador
    (when (>= posFound 0)
        (setq i posFound)
        (loop
            (setf (aref gameArray i) (aref gameArray (+ i 1)))
            (setq i (+ i 1))
            (when (>= i (- gameCount 1)) (return))
        )
        ; limpiar ultima posicion y bajar contador
        (setf (aref gameArray (- gameCount 1)) nil)
        (setq gameCount (- gameCount 1))
    )
)