(require 'usocket)

(defparameter my-stream (usocket:socket-stream (usocket:socket-connect #(127 0 0 1) 4321)))

;; The rest of the exercise is almost exactly the same as the book

;; Write to server and force the buffer onto the stream
(print "Yo! Server!" my-stream)
(force-output my-stream)

;; Read from server
(read my-stream)

;; And done
(close my-stream)
