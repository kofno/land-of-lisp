(require 'usocket)

(defparameter my-socket (usocket:socket-listen #(127 0 0 1) 4321))
(defparameter my-stream (usocket:socket-stream (usocket:socket-accept my-socket)))

;; The rest of the exercise is almost exactly the same as the book

;; Read from the client
(read my-stream)

;; Respond and force the buffer on the stream
(print "What up, Client?" my-stream)
(force-output my-stream)

;; Complete
(close my-stream)

