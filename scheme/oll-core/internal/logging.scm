(define-module (oll-core internal logging))

(use-modules
 (lily)
 (oll-core internal alist-access))

; Constant symbols representing the different log levels.
(define oll-loglevels
  '((nolog . 0)
    (critical . 1)
    (warning . 2)
    (log . 3)
    (debug . 4)))

; Define one single public variable.
; We can't use oll-core's options for this because they are not loaded yet -
; and the option handline needs the logging code ...
; Initialize to 'log, will later be set to 'warning
(define oll-loglevel 3)

; Check if a logging function should be executed
; by comparing the value passed in <loglevel> to the
; currently active log level
(define (do-log loglevel)
  (>= oll-loglevel (assq-ref oll-loglevels loglevel)))

(define (set-log-level level)
  (let ((new-level (assq level oll-loglevels)))
    (if new-level
        (set! oll-loglevel (cdr new-level))
        (oll:warn
         (*location*) "Not a valid openLilyLib log level: ~a. Ignoring" level))))

; Generic function to consistently format the output for the logging functions
(define (oll-format-log fmt vals)
  (apply format (format "\n\n~a\n" fmt) vals))

; Open log file
(define oll-logfile
  (open-output-file
   (format "~a.oll.log" (ly:parser-output-name (*parser*)))))

; Generic function to consistently write to log file.
; <title> is a sectioning header in the log file
; <fmt> and <vals> are simply passed along.
(define (log-to-file title fmt vals)
  (format oll-logfile
    (string-append
     "\n"
     (os-path-join-os (location->normalized-path (*location*)))
     "\nLine: "
     (number->string (cadr (ly:input-file-line-char-column (*location*))))

     "\n~a:\n"
     (apply format fmt vals)
     "\n\n")
    title))


; Critical error
; Aborts the compilation of the input file
; so use with care!
(define (oll-error fmt . vals)
   (if (do-log 'critical)
       (begin
        ;log-to-file "Error" fmt vals)
        (ly:input-message (*location*)
         (format "Error:~a" (oll-format-log fmt vals)))
        (ly:error ""))))

(define (oll-warn fmt . vals)
   (if (do-log 'warning)
       (begin
        ;(oll:log-to-file "Warning" fmt vals)
        (ly:input-warning (*location*)
           (oll-format-log fmt vals)))))

(define (oll-log fmt . vals)
  (if (do-log 'log)
      (begin
       ;        (log-to-file "Event" fmt vals)
       (ly:input-message (*location*)
         (oll-format-log fmt vals)))))

(define (oll-debug fmt . vals)
   (if (do-log 'debug)
       (begin
        ;(oll:log-to-file "Debug info" fmt vals)
        (ly:input-message (*location*)
          (oll-format-log fmt vals)))))


(export set-log-level)
(export oll-error)
(export oll-warn)
(export oll-log)
(export oll-debug)