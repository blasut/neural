
(defparameter *learningrate1* 0.1)
(defparameter *learningrate2* 0.05)

(defstruct network
  allowed-error
  num-input  
  num-hidden 
  num-output 
  inputs 
  hidden
  outputs 
  w1
  w2
  output-errors 
  hidden-errors 
  input-training-examples 
  output-training-examples)


(defun init-network (numInput numHidden numOutput)
  (let ((network (make-network
                  :allowed-error 0.05
                  :num-input numInput
                  :num-hidden numHidden
                  :num-output numOutput
                  :inputs (make-array '(3 3))
                  :hidden (make-array '(3 3))
                  :outputs (make-array '(3 3))
                  :w1 (make-array '(3 3))
                  :w2 (make-array '(3 3))
                  :output-errors (make-array '(3 3))
                  :hidden-errors (make-array '(3 3))
                  :input-training-examples (make-array 1)
                  :output-training-examples (make-array 1))))

    (dotimes (i numInput)
      (dotimes (h numHidden)
        (setf (aref (network-w1 network) i h)
              (* 0.05 (- 0.5 (random 1.0))))))

    (dotimes (h numHidden)
      (dotimes (o numOutput)
        (setf (aref (network-w2 network) h o)
              (* 0.005 (- 0.5 (random 1.0))))))

    network))

    
(defun add-training-example (network inputs outputs)
  (setf (aref (network-input-training-examples))



(defun train (network)
  ())

(defun recall (network arr)
  ())









(defun test-nn ()
  (let ((network (init-network 3 3 3)))
    (add-training-example network #(0.1 0.1 0.9) #(0.9 0.1 0.1))
    (add-training-example network #(0.1 0.9 0.1) #(0.1 0.1 0.9))
    (add-training-example network #(0.9 0.1 0.1) #(0.1 0.9 0.1))
    (train network)
    (print (recall network #(0.08 0.2 0.88)))
    (print (recall network #(0.93 0.2 0.11)))
    (print (recall network #(0.11 0.9 0.06)))))

