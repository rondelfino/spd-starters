;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 03-HtDWquiz-TextEditor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A one line text editor that has basic functionality
;;                        shows text in a textbox when typing
;;                        can change position of the cursor using arrow keys
;;                        can insert characters anywhere in the string
;;                        delete preceding characters using backspace and characters following the cursor using delete

;; =================
;; Constants:
(define TWIDTH 380)
(define THEIGHT 25)

(define FONT-SIZE 20)
(define TEXT-START-OFFSET -5)

(define TEXTBOX (rectangle TWIDTH THEIGHT "outline" "black"))

(define CURSOR (rectangle 2 20 "solid" "red"))


;; =================
;; Data definitions:
(define-struct tbs (pre post))
;; TextboxState is a (make-tbs String String Boolean)
;;                 pre is the string before the cursor
;;                 post is the string after the cursor
(define TBS1 (make-tbs "" ""))      ;cursor is at the end of pre -> ""|
(define TBS2 (make-tbs "hello" "")) ;cursor is at end of "hello" -> "hello|"
(define TBS3 (make-tbs "hel" "lo")) ;cursor is in between "hel" and "lo" -> "hel|lo"
(define TBS4 (make-tbs "" "hello")) ;cursor is at the beginning of "hello" -> "|hello"

#;
(define (fn-for-tbs tbs)
  (... (tbs-pre tbs)
       (tbs-post tbs)))

;; Template rules used:
;; - compound: 2 fields



;; =================
;; Functions:

;; TextboxState -> TextboxState
;; start the world with an empty textbox, with the cursor at the start (main (make-tbs "" ""))
;; 
(define (main tbs)
  (big-bang tbs                     ; TextboxState
            (to-draw   render-tbs)  ; TextboxState -> Image
            (on-key    handle-key))); TextboxState KeyEvent -> TextboxState


;; TextboxState -> Image
;; updates the textbox to display the pre and post strings, as well as the cursor between them
(check-expect (render-tbs (make-tbs "" ""))
              (overlay/align/offset "left" "middle"
                                    (beside (text "" FONT-SIZE "black") CURSOR (text "" FONT-SIZE "black"))
                                    TEXT-START-OFFSET 0
                                    TEXTBOX))
(check-expect (render-tbs (make-tbs "hello" ""))
              (overlay/align/offset "left" "middle"
                                    (beside (text "hello" FONT-SIZE "black") CURSOR (text "" FONT-SIZE "black"))
                                    TEXT-START-OFFSET 0
                                    TEXTBOX))
(check-expect (render-tbs (make-tbs "hel" "lo"))
              (overlay/align/offset "left" "middle"
                                    (beside (text "hel" FONT-SIZE "black") CURSOR (text "lo" FONT-SIZE "black"))
                                    TEXT-START-OFFSET 0
                                    TEXTBOX))
(check-expect (render-tbs (make-tbs "" "hello"))
              (overlay/align/offset "left" "middle"
                                    (beside (text "" FONT-SIZE "black") CURSOR (text "hello" FONT-SIZE "black"))
                                    TEXT-START-OFFSET 0
                                    TEXTBOX))

;(define (render-tbs tbs) (text "1" FONT-SIZE "black")) ;stub

;; <use template from TextboxState>

;; String String -> Image
;; consumes two strings and outputs an image of a textbox with a cursor between the two strings
(define (format-textbox pre post)
  (overlay/align/offset "left" "middle"
                        (beside (text pre FONT-SIZE "black") CURSOR (text post FONT-SIZE "black"))
                        TEXT-START-OFFSET 0
                        TEXTBOX))

(define (render-tbs tbs)
  (overlay/align/offset "left" "middle"
                        (beside (text (tbs-pre tbs) FONT-SIZE "black") CURSOR (text (tbs-post tbs) FONT-SIZE "black"))
                        TEXT-START-OFFSET 0
                        TEXTBOX))


;; TextboxState KeyEvent -> TextboxState
;; updates the (tbs-pre tbs) string with the corresponding keypress
;; deletes a character from (tbs-pre tbs) string when the backspace is pressed
;; updates (tbs-post tbs) and (tbs-pre tbs) when the left and right arrow keys are pressed
;; !!!
(check-expect (handle-key (make-tbs "" "") "A") (make-tbs "A" ""))              ;start
(check-expect (handle-key (make-tbs "hell" "o") "a") (make-tbs "hella" "o"))    ;cursor in the middle
(check-expect (handle-key (make-tbs "hell" "o") "\b") (make-tbs "hel" "o"))     ;backspace deletes characters from (tbs-pre tbs)
(check-expect (handle-key (make-tbs "hell" "o") "shift") (make-tbs "hell" "o")) ;ignores modifiers like shift, alt, and any other key that isn't 1 character long
(check-expect (handle-key (make-tbs "hel" "lo") "left") (make-tbs "he" "llo"))
(check-expect (handle-key (make-tbs "hel" "lo") "right") (make-tbs "hell" "o"))
(check-expect (handle-key (make-tbs "" "hello") "left") (make-tbs "" "hello"))
(check-expect (handle-key (make-tbs "hello" "") "right") (make-tbs "hello" ""))


;(define (handle-key tbs ke) (make-tbs "" "")) ;stub

;; <use template from KeyHandler>


(define (handle-key tbs ke)
  (cond [(key=? "\b" ke) (make-tbs (string-butlast (tbs-pre tbs)) (tbs-post tbs))]
        [(= (string-length ke) 1) (make-tbs (string-append (tbs-pre tbs) ke) (tbs-post tbs))]
        [(key=? "right" ke)
         (make-tbs (string-append (tbs-pre tbs) (string-first (tbs-post tbs)))
                   (string-butfirst (tbs-post tbs)))]
        [(key=? "left" ke)
         (make-tbs (string-butlast (tbs-pre tbs))
                   (string-append (string-last (tbs-pre tbs)) (tbs-post tbs)))]
        [else tbs]))


;; String -> String
;; consumes a string, and returns the string with the last character deleted
(check-expect (string-butlast "hello") "hell")
(check-expect (string-butlast "") "")
(check-expect (string-butlast "a") "")

;(define (string-butlast s) " ") ;stub

#;
(define (fn-for-string-butlast s)
  (... s))

(define (string-butlast s)
  (if (> (string-length s) 0)
      (substring s 0 (- (string-length s) 1))
      s))


;; String -> String
;; consumes a string and returns the last character of the string as a string
(check-expect (string-last "") "")
(check-expect (string-last "a") "a")
(check-expect (string-last "hello") "o")
(check-expect (string-last "\thello") "o")
(check-expect (string-last " hello ") " ")

;(define (string-last s) " ") ;stub

#;
(define (fn-for-string-last s)
  (... s))

(define (string-last s)
  (if (> (string-length s) 0)
      (substring s (- (string-length s) 1) (string-length s))
      s))


;; String -> String
;; consumes a string, and returns the first character of the string as a string
(check-expect (string-first "") "")
(check-expect (string-first "a") "a")
(check-expect (string-first "hello") "h")
(check-expect (string-first "\thello") "\t")
(check-expect (string-first " hello") " ")

;(define (string-first s) " ") ;stub

#;
(define (fn-for-string-first s)
  (... s))

(define (string-first s)
  (if (> (string-length s) 0)
      (substring s 0 1)
      s))


;; String -> String
;; consumes a string and returns all but the first character of the string
(check-expect (string-butfirst "") "")
(check-expect (string-butfirst "a") "")
(check-expect (string-butfirst "hello") "ello")
(check-expect (string-butfirst "\thello") "hello")
(check-expect (string-butfirst " hello ") "hello ")

;(define (string-butfirst s) " ") ;stub

#;
(define (fn-for-string-butfirst s)
  (... s))

(define (string-butfirst s)
  (if (> (string-length s) 0)
      (substring s 1 (string-length s))
      s))
