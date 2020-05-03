;nyquist plug-in
;version 3
;type analyze
;name "Timeslotted QPSK bit slicer"
;control symbolrate "Symbol rate" float "Hz" 18000 0 1000000
;control nsyms "Symbols per slot" int "symbols" 255 0 1000000

;; Divide signal into fixed-length timeslots and estimate symbol timing
;; independently for each slot. Print one slot per line.

(setq oversamp 8)
(setq nsamps (* nsyms oversamp))

(let ((resampled (force-srate (* oversamp symbolrate) s)))
 (loop
  (let ((a-i (snd-fetch-array (aref resampled 0) nsamps nsamps))
        (a-q (snd-fetch-array (aref resampled 1) nsamps nsamps))
        (best-timing-phase 0) (best-sum 0))
   (unless a-i (return))  ;; Stop when there's no more signal
   (dotimes (tphase 8)
    (let ((s 0))
     (dotimes (symn nsyms)
       (setq s (+ s (abs (aref a-i (+ tphase (* 8 symn))))
                    (abs (aref a-q (+ tphase (* 8 symn))))))
     )
     (if (> s best-sum)
      (progn (setq best-timing-phase tphase)
             (setq best-sum s)))
     ;;(prin1 s) (princ " ")
    )
   )
   (dotimes (symn nsyms)
     (prin1 (if (< (aref a-q (+ best-timing-phase (* 8 symn))) 0) 1 0))
     (prin1 (if (< (aref a-i (+ best-timing-phase (* 8 symn))) 0) 1 0))
   )
  )
  (princ "\n")
 )
)

