;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "editor.rkt")
(provide
 make-editor
 editor-pre
 editor-post
 editor?
 edit)



;; DATA DEFINITION :
(define-struct editor(pre post))

;; CONSTRUCTOR TEMPLATE :
;; An editor is (make-editor String String)

;; INTERPRETATION :
;; (make-editor String String) is an editor where
;; pre is the text which appears before the cursor
;; post is the text appearing after the cursor

;; DESTRUCTOR TEMPLATE :
;; editor-fn : editor -> ??
#; (define (editor-fn ed)
     (...
      (editor-pre ed)
      (editor-post ed)))



;; A KeyEvent can have values from :
;; -- "left"
;; -- "right"
;; -- "\b"
;; -- single character

;; DESTRUCTOR TEMPLATE :
;; keyevent-fn : KeyEvent -> ??
#;(define (keyevent-fn ke)
    (...
     (string=? ke "left")
     (string=? ke "right")
     (string=? ke "\b")
     (else single character)))
   


;; move-cursor-left : String String -> Editor
;; GIVEN : Two strings, where the first string is the text before cursor(pre) and
;;         second string is the text after the cursor(post)
;; RETURNS : An editor, with editor-pre containing the text before cursor, editor-post
;;           containing text after cursor and the cursor has shifted left by 1 from its
;;           original position
;; EXAMPLES :
;; (move-cursor-left "MoveCursor" "Left") => (make-editor "MoveCurso" "rLeft")
;; (move-cursor-left "MoveCursorLeft" "") => (make-editor "MoveCursorLef" "t")
;; (move-cursor-left "" "MoveCursorLeft") => "No Characters before Cursor!"

;; STRATEGY : Divide into cases on <condition> 
(define (move-cursor-left pre post)
  (if (> (string-length pre) 0)
      (make-editor (substring pre 0 (- (string-length pre) 1))
                   (string-append (substring pre (- (string-length pre) 1))
                                  post))
      "No Characters before Cursor!"))

;; TESTS :
(begin-for-test
  (check-equal?
   (move-cursor-left "MoveCursor" "Left")
   (make-editor "MoveCurso" "rLeft")
   "Output should be (make-editor 'MoveCurso' 'rLeft')")
  (check-equal?
   (move-cursor-left "MoveCursorLeft" "")
   (make-editor "MoveCursorLef" "t")
   "Output should be (make-editor 'MoveCursorLef' 't')")
  (check-equal?
   (move-cursor-left "" "MoveCursorLeft")
   "No Characters before Cursor!")
  "Output should be 'No Characters before Cursor!")
   


;; move-cursor-right : String String -> Editor
;; GIVEN : Two strings, where the first string is the text before cursor(pre) and
;;         second string is the text after the cursor(post)
;; RETURNS : An editor, with editor-pre containing the text before cursor, editor-post
;;           containing text after cursor and the cursor has shifted right by 1 from its
;;           original position
;; EXAMPLES :
;; (move-cursor-right "MoveCursor" "Right") => (make-editor "MoveCursorR" "ight")
;; (move-cursor-right "" "MoveCursorRight") => (make-editor "M" "oveCursorRight")
;; (move-cursor-right "MoveCursorRight" "") => "No characters after Cursor!"

;; STRATEGY : Divide into cases on <condition> 
(define (move-cursor-right pre post)
  (if (> (string-length post) 0)
  (make-editor (string-append pre (string-ith post 0))
               (substring post 1 (string-length post)))
  "No characters after Cursor!"))

;; TESTS :
(begin-for-test
  (check-equal?
   (move-cursor-right "MoveCursor" "Right")
   (make-editor "MoveCursorR" "ight")
   "Output should be (make-editor 'MoveCursorR' 'ight')")
  (check-equal?
   (move-cursor-right "" "MoveCursorRight")
   (make-editor "M" "oveCursorRight")
   "Output should be (make-editor 'M' 'oveCursorRight')")
  (check-equal?
   (move-cursor-right "MoveCursorRight" "")
   "No characters after Cursor!")
  "Output should be 'No characters after Cursor!'")



;; remove-character : String String -> Editor 
;; GIVEN : Two strings, where the first string is the text before cursor(pre) and
;;         second string is the text after the cursor(post)
;; RETURNS : An editor, with editor-pre containing the text with 1 character deleted from end,
;;           post containing text after cursor and the cursor has shifted left by 1 from its
;;           original position
;; EXAMPLES :
;; (remove-character "Delete" "Character") => (make-editor "Delet" "Character")
;; (remove-character "DeleteCharacter" "") => (make-editor "DeleteCharacte" "")
;; (remove-character "" "DeleteCharacter") => "Can't perform backspace! No characters before Cursor!"

;; STRATEGY : Divide into cases on <condition> 
(define (remove-character pre post)
  (if (> (string-length pre) 0)
      (make-editor (substring pre 0 (- (string-length pre) 1))
                  post)
      "Can't perform backspace! No characters before Cursor!"))

;; TESTS :
(begin-for-test
  (check-equal?
   (remove-character "Delete" "Character")
   (make-editor "Delet" "Character")
   "Output should be (make-editor 'Delet' 'Character')")
  (check-equal?
   (remove-character "DeleteCharacter" "")
   (make-editor "DeleteCharacte" "")
   "Output should be (make-editor 'DeleteCharacte' '')")
  (check-equal?
   (remove-character "" "DeleteCharacter")
   "Can't perform backspace! No characters before Cursor!")
  "Output should be 'Can't perform backspace! No characters before Cursor!'")



;; insert-character : String String String -> Editor
;; GIVEN : Three strings, with
;;         first string being the text before cursor(pre),
;;         second string being the text after cursor(post) and
;;         third string being a single character which has to be inserted after cursor(str)
;; RETURNS : An editor, where editor-pre has the input character(str) appended to the end of
;;           first input string(pre) and editor-post is same as the second input string(post)
;; EXAMPLES :
;; (insert-character "Ad" "Character" "d") => (make-editor "Add" "Character")
;; (insert-character "" "ddCharacter" "A") => (make-editor "A" "ddCharacter")
;; (insert-character "AddCharacte" "" "r") => (make-editor "AddCharacter" "")

;; STRATEGY : Use of editor template
(define (insert-character pre post str)
  (make-editor (string-append pre str) post))

;; TESTS :
(begin-for-test
  (check-equal?
   (insert-character "Ad" "Character" "d")
   (make-editor "Add" "Character")
   "Output should be (make-editor 'Add' 'Character')")
  (check-equal?
   (insert-character "" "ddCharacter" "A")
   (make-editor "A" "ddCharacter")
   "Output should be (make-editor 'A' 'ddCharacter')")
  (check-equal?
   (insert-character "AddCharacte" "" "r")
   (make-editor "AddCharacter" "")
   "Output should be (make-editor 'AddCharacter' '')"))



;; edit : Editor String -> Editor
;; GIVEN : An editor and a string which denotes the KeyEvent
;; RETURNS : An editor with given KeyEvent performed on input editor
;; EXAMPLES :
;; (edit (make-editor "MoveCursor" "Left") "left") => (make-editor "MoveCurso" "rLeft")
;; (edit (make-editor "MoveCursor" "Right") "right") => (make-editor "MoveCursorR" "ight")
;; (edit (make-editor "Delete" "Character") "\b") => (make-editor "Delet" "Character")
;; (edit (make-editor "Ad" "Character") "d") => (make-editor "Add" "Character")

;; STRATEGY : Use editor and keyevent template
(define (edit ed KeyEvent)
  (cond
    [(string=? KeyEvent "left")
     (move-cursor-left (editor-pre ed) (editor-post ed))]
    [(string=? KeyEvent "right")
     (move-cursor-right (editor-pre ed) (editor-post ed))]
    [(string=? KeyEvent "\b")
     (remove-character (editor-pre ed) (editor-post ed))]
    [else (insert-character (editor-pre ed) (editor-post ed) KeyEvent)]))
   
;; TESTS :
(begin-for-test
  (check-equal? 
   (edit (make-editor "MoveCursor" "Left") "left")
   (make-editor "MoveCurso" "rLeft")
   "Output should be (make-editor 'MoveCurso' 'rLeft')")
  (check-equal? 
   (edit (make-editor "MoveCursorLeft" "") "left")
   (make-editor "MoveCursorLef" "t")
   "Output should be (make-editor 'MoveCursorLef' 't')")
  (check-equal? 
   (edit (make-editor "" "MoveCursorLeft") "left")
   "No Characters before Cursor!"
   "Output should be 'No Characters before Cursor!'")
  (check-equal? 
   (edit (make-editor "MoveCursor" "Right") "right")
   (make-editor "MoveCursorR" "ight")
   "Output should be (make-editor 'MoveCursorR' 'ight')")
  (check-equal? 
   (edit (make-editor "" "MoveCursorRight") "right")
   (make-editor "M" "oveCursorRight")
   "Output should be (make-editor 'M' 'oveCursorRight')")
  (check-equal? 
   (edit (make-editor "MoveCursorRight" "") "right")
   "No characters after Cursor!"
   "Output should be 'No characters after Cursor!'")
  (check-equal?
   (edit (make-editor "Delete" "Character") "\b")
   (make-editor "Delet" "Character")
   "Output should be (make-editor 'Delet' 'Character')")
  (check-equal?
   (edit (make-editor "DeleteCharacter" "") "\b")
   (make-editor "DeleteCharacte" "")
   "Output should be (make-editor 'DeleteCharacte' '')")
  (check-equal?
   (edit (make-editor "" "DeleteCharacter") "\b")
   "Can't perform backspace! No characters before Cursor!"
   "Output should be 'Can't perform backspace! No characters before Cursor!'")
  (check-equal?
   (edit (make-editor "Ad" "Character") "d")
   (make-editor "Add" "Character")
   "Output should be (make-editor 'Add' 'Character')")
  (check-equal?
   (edit (make-editor "" "ddCharacter") "A")
   (make-editor "A" "ddCharacter")
   "Output should be (make-editor 'A' 'ddCharacter')")
  (check-equal?
   (edit (make-editor "AddCharacte" "") "r")
   (make-editor "AddCharacter" "")
   "Output should be (make-editor 'AddCharacter' '')"))