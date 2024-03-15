(ql:quickload "cl-base64")

(defun number-to-card (number)
  (let* ((suit-index (floor (/ number 13)))
         (rank-index (+ 1 (mod number 13))))
    (cond ((= number 52) '(#\J #\A))
          ((= number 53) '(#\J #\B))
          (t (list
              (case suit-index
                (0 #\♣)
                (1 #\♦)
                (2 #\♥)
                (3 #\♠))
              (case rank-index
                (1 #\A)
                (11 #\J)
                (12 #\Q)
                (13 #\K)
                (otherwise rank-index)))))))

;; Функция для запуска внешней команды и получения ее вывода
(defun run-command (command)
  (string-trim '(#\Newline) (uiop:run-program command :output :string)))

(defun number-to-byte-array (number)
  (let ((byte-array (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (i 4)
      (setf (aref byte-array i)
            (ldb (byte 8 (* i 8)) number)))
    byte-array))

(defun number-to-byte-array (number &optional (bits 8))
  (let* ((byte-size (ceiling (log number 256)))
         (byte-array (make-array byte-size :element-type '(unsigned-byte 8))))
    (dotimes (i byte-size)
      (setf (aref byte-array i)
            (ldb (byte bits 0)
                 (ldb (byte 8 (* i 8)) number))))
    byte-array))

(defun get-next-rnd-lst (rnd-cmd &optional bits)
  (let* ((urnd (run-command rnd-cmd))
         (urnd-number (cl-base64:base64-string-to-integer urnd)))
    (coerce (number-to-byte-array urnd-number bits) 'list)))

;; (get-next-rnd-lst "cat /dev/urandom | base64 | head -n 1" 6)

(defun get-random-hand ()
  (let* ((result '())
         (command "cat /dev/urandom | base64 | head -n 1")
         (source (get-next-rnd-lst command 6)))
    (loop
      until (= (length result) 54)
      do (when (null source)
           (setf source (get-next-rnd-lst command 6))
           (print 'refresh))
         (let ((item (pop source)))
           (print item)
           (when (< item 54)
             (pushnew item result))))
    result))

(print
 (mapcar #'(lambda (x)
             (format nil "~A:~A" (number-to-card x) x))
         (get-random-hand)))
