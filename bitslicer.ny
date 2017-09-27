;nyquist plug-in
;version 3
;type process
;name "Bit slicer"

;;control filename "Filename" string ""
;control symbol-rate "Symbol rate" float "Hz" 10000 0 1000000
;;control do-remove-dc "Remove DC" choice "No,Yes" 1
;control do-synchronize "Synchronize" choice "No,Yes" 1
(setq oversample (if (= do-synchronize 1) 8 1))

(defun sample-symbols (p)
  (let* ((t0 (snd-t0 p))
         (tlen (/ (snd-length p 999999999) (snd-srate p)))
         (symbol-sample-rate (* symbol-rate oversample))
         (nsyms (round (* tlen symbol-sample-rate)))
         (symtime (/ 1 symbol-sample-rate)) 
         (samples (make-array nsyms))
        )
    (dotimes (i nsyms)
      (setf (aref samples i) (snd-sref p (+ (* i symtime) t0)))
;;    (setf (aref samples i) (if (> (snd-sref p (+ (* i symtime) t0)) 0) 1 0))
    )
    samples
  )
)

(defun timing-estimate (p nbits)
  (let ((best-timing-phase 0) (best-sum 0))
    (dotimes (timing-phase oversample)
      (let ((abs-sum 0))
        (dotimes (i nbits) (setq abs-sum (+ abs-sum (abs (aref p (+ (* i oversample) timing-phase))))))
        (if (> abs-sum best-sum)
          (progn (setq best-timing-phase timing-phase)
                 (setq best-sum abs-sum)))
;;      (print abs-sum)
      )
    )
    best-timing-phase ))

(let ((ss (sample-symbols s))
;;    (f (open filename :direction :output))
      (f (open "bits" :direction :output))
     )
  (let* ((nbits (truncate (/ (length ss) oversample)))
         (timing-phase (if (= do-synchronize 1) (timing-estimate ss nbits) 0)))
    (dotimes (i nbits)
      (format f "~a" (if (> (aref ss (+ (* i oversample) timing-phase)) 0) 1 0))
    )
  )
)
