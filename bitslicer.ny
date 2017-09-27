;nyquist plug-in
;version 3
;type process
;name "Bit slicer"

;;control filename "Filename" string ""
;control symbol-rate "Symbol rate" float "Hz" 10000 0 1000000
;;control do-remove-dc "Remove DC" choice "No,Yes" 1
;;control do-synchronize "Synchronize" choice "No,Yes" 1
;;(setq oversample (if ((= do-synchronize 1) 8 1))
(setq oversample 1)

(defun sample-symbols (p)
  (let* ((t0 (snd-t0 p))
         (tlen (/ (snd-length p 999999999) (snd-srate p)))
         (symbol-sample-rate (* symbol-rate oversample))
         (nsyms (round (* tlen symbol-sample-rate)))
         (symtime (/ 1 symbol-sample-rate)) 
         (samples (make-array nsyms))
        )
    (dotimes (i nsyms)
;;      (setf (aref samples i) (sref p (* i symtime)))
      (setf (aref samples i) (if (> (snd-sref p (+ (* i symtime) t0)) 0) 1 0))
    )
    samples
  )
)

(let ((ss (sample-symbols s))
;;    (f (open filename :direction :output))
      (f (open "bits" :direction :output))
     )
  (dotimes (i (length ss))
    (format f "~a" (aref ss i))
  )
)
