(require :sb-bsd-sockets)

;;basics of using sockets from server perspective

(defparameter *server* (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
(sb-bsd-sockets:socket-bind *server* (vector 127 0 0 1) 8000)
(sb-bsd-sockets:socket-listen *server* 1)
(defparameter *comm-sock* (sb-bsd-sockets:socket-accept *server*))
;;at this point the server waits for an incoming connection
(defparameter *comm-stream* (sb-bsd-sockets:socket-make-stream *comm-sock* :input t :output t :buffering :none))
;;now use stream to communicate with client
;;to get file descriptor of socket for select/poll functionality
(sb-bsd-sockets:socket-file-descriptor *server*)

;;basics of client
(defparameter *client* (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
(sb-bsd-sockets:socket-connect *client* (vector 127 0 0 1) 8000)
;;at this point client is connected
(defparameter *comm-stream* (sb-bsd-sockets:socket-make-stream *client* :input t :output t :buffering :none))
;;use stream to communicate with server
