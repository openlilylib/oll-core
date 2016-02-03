% Code from module-handling.ily
% This had been used for the declaration of libraries,
% and it has to be discussed to what extent this will be used together
% with lyp. Either here, there or nowhere.

%{

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for type-checking of library options

% Simple regex check for Name plus email address in angled brackets:
% "Ben Maintainer <ben@maintainer.org>"
#(define (oll-maintainer? obj)
   (let ((pat (make-regexp ".*<.*@.*>")))
     (if (and (string? obj)
              (regexp-exec pat obj))
         #t #f)))

% Returns true for one maintainer or a list of them
#(define (oll-maintainers? obj)
   (or (oll-maintainer? obj)
       (and (list? obj)
            (every oll-maintainer? obj))))

% Returns true if obj is a string representation of an integer
#(define (integer-string? obj)
   (integer? (string->number obj)))

% Returns true if a string is a three-element dot-joined list of integers
#(define (oll-version-string? obj)
   (and (string? obj)
        (let ((lst (string-split obj #\.)))
          (and (= 3 (length lst))
               (every integer-string? lst)))))

% Alist with mandatory options for library declarations
% Each entry is a pair of option name symbol and type predicate
#(define oll-lib-mandatory-options
   `((maintainers . ,oll-maintainers?)
     (version . ,oll-version-string?)
     (short-description . ,string?)
     (description . ,string?)
     ))

% Alist with recognized options for library declarations
% If an option is in this list it is type-checked against the given predicate.
#(define oll-lib-known-options
   `((lilypond-min-version . ,oll-version-string?)
     (lilypond-max-version . ,oll-version-string?)
     ))


% Declare a library, to be done in the __init__.ily file
% Arguments:
% - display-name: The official name of the library
% - name (optional): the directory name of the library
%   This name must be 'symbol?' compatible, i.e. must consist of
%   alphabetical characters and hyphens only.
%   This argument can be omitted if the display-name is the same
%   as the directory name with exception of capitalization.
%   (e.g. when the display-name is "ScholarLY" the implicit 'name'
%    is "scholarly").
% - options: a \with {} clause with metadata options.
%   some of them are mandatory, others can be used at the discretion
%   of the library maintainers:
%   For possible mandatory and known options see the two lists above.
%
declareLibrary =
#(define-void-function (parser location display-name name options)
   (string? (symbol?) ly:context-mod?)
   (let*
    ;; internal-name is either explicitly given
    ;; or the lowercase version of display-name
    ((internal-name
      (or name (string-downcase display-name)))
     ;; option path to the library's meta options
     (meta-path `(,(string->symbol internal-name) meta))
     ;; retrieve options from context mods
     (options (extract-options options)))

    ;; initialize library's meta option branch
    #{ \registerOption #meta-path #'() #}

    ;; check if all mandatory options are present
    (for-each
     (lambda (o)
       (let ((mand-opt (car o)))
         (if (not (assoc-ref options mand-opt))
             (oll:error (format "
    Missing option in library declaration!
    Library: \"~a\"
    Option: \"~a\"" display-name mand-opt) ""))
         ))
     oll-lib-mandatory-options)

    ;; process options, type-check mandatory options and store in meta
    (for-each
     (lambda (o)
       (let* ((opt-name (car o))
              (opt-val (cdr o))
              (predicate? (assoc-ref oll-lib-mandatory-options opt-name))
              (known-opt-pred? (assoc-ref oll-lib-known-options opt-name)))
         ;; check for type if there is a predicate (-> true for mandatory options)
         (if (and predicate?
                  (not (predicate? opt-val)))
             (oll:error (format "
    Type check failed for mandatory option in library declaration!
    Library: \"~a\"
    Option: \"~a\"
    Predicate: ~a" display-name opt-name predicate?) ""))
         (if (and known-opt-pred?
                  (not (known-opt-pred? opt-val)))
             (oll:error (format "
    Type check failed for known option in library declaration!
    Library: \"~a\"
    Option: \"~a\"
    Predicate: ~a" display-name opt-name known-opt-pred?) ""))

         ;; store option
         #{ \setChildOption #meta-path #opt-name #opt-val #}
         ))
     options)))

%}