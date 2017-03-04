
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
                  :inputs (make-array 3 :initial-element 0)
                  :hidden (make-array 3 :initial-element 0)
                  :outputs (make-array 3 :initial-element 0)
                  :w1 (make-array '(3 3))
                  :w2 (make-array '(3 3))
                  :output-errors (make-array '(3 3))
                  :hidden-errors (make-array '(3 3))
                  :input-training-examples (make-array 1 :adjustable t :fill-pointer 0)
                  :output-training-examples (make-array 1 :adjustable t :fill-pointer 0))))

    (dotimes (i numInput)
      (dotimes (h numHidden)
        (setf (aref (network-w1 network) i h)
              (* 0.01 (- 0.005 (random 1.0))))))

    (dotimes (h numHidden)
      (dotimes (o numOutput)
        (setf (aref (network-w2 network) h o)
              (* 0.005 (- 0.0025 (random 1.0))))))

    network))

    
(defun add-training-example (network inputs outputs)
  (progn (vector-push-extend inputs (network-input-training-examples network))
         (vector-push-extend outputs (network-output-training-examples network))
         network))

(defun sigmoid (x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

(defun sigmoidP (x)
  (let ((z (sigmoid x)))
    (* z (- 1.0 z))))

(defun forward-pass (n)
  (progn
    (setf (network-hidden n) (make-array (network-num-hidden n) :initial-element 0))
    (setf (network-outputs n) (make-array (network-num-output n) :initial-element 0))

    (dotimes (i (network-num-input n))
      (dotimes (h (network-num-hidden n))
        (setf (aref (network-hidden n) h)
              (+ (aref (network-hidden n) h)
                 (* (aref (network-inputs n) i)
                    (aref (network-w1 n) i h))))))

    (dotimes (h (network-num-hidden n))
      (dotimes (o (network-num-output n))
        (setf (aref (network-outputs n) o)
              (+ (aref (network-outputs n) o)
                 (* (sigmoid (aref (network-hidden n) h))
                    (aref (network-w2 n) h o))))))

    (dotimes (o (network-num-output n))
      (setf (aref (network-outputs n) o)
            (sigmoid (aref (network-outputs n) o))))

    n))

(defun reset-weights (n)
  (progn
    (format t "input: ~d, hidden: ~d, output: ~d ~%"
            (network-num-input n)
            (network-num-hidden n)
            (network-num-output n)) 

    (dotimes (i (network-num-input n))
      (dotimes (h (network-num-hidden n))
        (setf (aref (network-w1 n) i h)
              (* 0.025 (- (random 1.0) 0.5)))))

    (dotimes (h (network-num-hidden n))
      (dotimes (o (network-num-output n))
        (setf (aref (network-w2 n) h o)
              (* 0.005 (- (random 1.0) 0.5)))))

    n))


(defun train-helper (n)
  (let ((error-count 0)
        (num-cases (length (network-input-training-examples n)))
        (outs '()))
    (progn
      (dotimes (ncase num-cases)
        (progn
          (setf (network-hidden-errors n)
                (make-array (network-num-hidden n) :initial-element 0))
          (setf (network-output-errors n)
                (make-array (network-num-output n) :initial-element 0))
          (dotimes (i (network-num-input n))
            (setf (aref (network-inputs n) i)
                  (aref (aref (network-input-training-examples n) ncase) i)))

          (setf outs (aref (network-output-training-examples n) ncase))

          (forward-pass n)

          (dotimes (o (network-num-output n))
            (setf (aref (network-output-errors n) o)
                  (* (- (aref outs o) (aref (network-outputs n) o))
                     (sigmoidP (aref (network-outputs n) o)))))

          (dotimes (h (network-num-hidden n))
            (progn
              (setf (aref (network-hidden-errors n) h) 0.0)
              (dotimes (o (network-num-output n))
                (setf (aref (network-hidden-errors n) h)
                      (+ (aref (network-hidden-errors n) h)
                         (* (aref (network-output-errors n) o)
                            (aref (network-w2 n) h o)))))))

          (dotimes (h (network-num-hidden n))
            (setf (aref (network-hidden-errors n) h)
                  (* (aref (network-hidden-errors n) h)
                     (sigmoidP (aref (network-hidden n) h)))))

          (dotimes (o (network-num-output n))
            (dotimes (h (network-num-hidden n))
              (setf (aref (network-w2 n) h o)
                    (+ (aref (network-w2 n) h o)
                       (* *learningrate2*
                          (aref (network-output-errors n) o)
                          (aref (network-hidden n) h))))))

          (dotimes (h (network-num-hidden n))
            (dotimes (i (network-num-input n))
              (setf (aref (network-w1 n) i h)
                    (+ (aref (network-w1 n) i h)
                       (* *learningrate1*
                          (aref (network-hidden-errors n) h)
                          (aref (network-inputs n) i))))))

          (dotimes (o (network-num-output n))
            (setf error-count (+ error-count
                                 (abs (- (aref outs o)
                                         (aref (network-outputs n) o)))))))))
    error-count))
  

(defun train (n)
  (dotimes (iter 150000)
    (let ((error-count (train-helper n)))
      (if (= (mod iter 800) 0)
          (print error-count))
      (if (< error-count 0.05)
          (return))
      (if (and (> error-count 2.0) (= 0 (mod iter 7111)))
          (progn (print "reset weights")
                 (reset-weights n))))))


(defun recall (n inputs)
  (progn
    (setf (network-inputs n) inputs)
    (forward-pass n)
    (network-outputs n)))

(defun test-nn ()
  (let ((network (init-network 3 3 3)))
    (add-training-example network #(0.1 0.1 0.9) #(0.9 0.1 0.1))
    (add-training-example network #(0.1 0.9 0.1) #(0.1 0.1 0.9))
    (add-training-example network #(0.9 0.1 0.1) #(0.1 0.9 0.1))
    (train network)
    (print (recall network #(0.08 0.2 0.88)))
    (print (recall network #(0.93 0.2 0.11)))
    (print (recall network #(0.11 0.9 0.06)))
    network))

