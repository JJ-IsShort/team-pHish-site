(load "website-builder.lisp")

(in-package :cl-user)
(defpackage :pHish-site
  (:use :cl :website-generator))

(in-package :pHish-site)

(defun generate-wave-clip-path (&key (amplitude 10) 
                                      (frequency 3) 
                                      (phase 0)
                                      (base-x 50)
                                      (points 50))
  (let ((coords nil)
        (step (/ 100.0 (1- points))))
    
    (dotimes (i points)
      (let* ((y (* i step))
             (angle (+ phase (* 2 pi frequency (/ y 100.0))))
             (x (+ base-x (* amplitude (sin angle)))))
        (push (format nil "~,2f% ~,2f%" x y) coords)))
    
    (push "100% 100%" coords)
    (push "100% 0%" coords)
    
    (format nil "polygon(~{~a~^, ~})" (nreverse coords))))

(define-component sidebar ()
  "Sidebar with cool opening animation"
  `(div (:id "sidebar"
         :class "fixed translate-x-full top-0 right-0 h-screen
         ease-out delay-150 duration-300 w-[333px]")
     (div (:class "size-full relative")
        (div (:class "size-[3rem] top-[3rem] -left-[6rem] absolute"
              :id "sidebar-button")
            (div (:class "relative")
                (button (:type "button"
                          :class "bg-blue size-[3rem]
                                  rounded-xl absolute cursor-pointer"
                          :onclick "press_menu()"))
                (button (:type "button"
                          :class "border-blue size-[3rem] 
                                  rounded-xl cursor-pointer border-8 absolute"
                          :onclick "press_menu()"))
                (button (:type "button"
                          :class "border-blue-light size-[3rem]
                                  rounded-xl cursor-pointer border-8 absolute top-[-0.1rem] right-[0.1rem]"
                          :onclick "press_menu()")))))
        (div (:class "w-[333px] h-screen fixed right-0 bottom-0")
           (div (:class "bg-blue-light w-[333px] h-screen
                 fixed right-10 bottom-0"
                 :style ,(format nil "clip-path: ~a;" (generate-wave-clip-path
                                                :amplitude 5
                                                :base-x 20))))
           (div (:class "bg-blue w-[333px] h-screen
                 fixed right-2 bottom-0"
                 :style ,(format nil "clip-path: ~a;" (generate-wave-clip-path
                                                :amplitude 5
                                                :base-x 20))))
           (div (:class "bg-blue-dark w-[333px] h-screen
                 fixed right-0 bottom-0"
                 :style ,(format nil "clip-path: ~a;" (generate-wave-clip-path
                                                :amplitude 5
                                                :base-x 20))))
           )
))

(write-page-to-file "index.html"
  `(page "Example"
    ((script (:src "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"))
     (style (:type "text/tailwindcss") "@theme {
            --color-*: initial;
            --color-blue-dark: #023047;
            --color-blue: #219ebc;
            --color-blue-light: #8ecae6;
            --color-white: #fff;
            --color-black: #000;
            --color-yellow-light: #FFEF05;
            --color-yellow: #FFB703;
            --color-yellow-dark: #FB8500;
            }")
     (script (:src "./script.js")))
    (div (:class "bg-yellow-dark w-screen h-screen")
         (div (:class "w-full h-full p-8 box-border")
              (div (:class "relative size-full")
                  (div (:class "bg-blue-dark size-full
                        rounded-[2rem] absolute flex flex-col")
                        (div (:class "size-fit m-auto text-yellow-light
                              font-mono text-black text-8xl")
                             "Team pHish"))
                  (div (:class "border-blue size-full
                        rounded-[2rem] border-8 absolute"))
                  (div (:class "border-blue-light size-full
                        rounded-[2rem] border-8 absolute top-[-0.1rem] right-[0.1rem]"))))
    )
    (div (:class "bg-black py-8 flex flex-row")
         (div (:class "flex m-auto flex-col w-[60%] flex-none")
               (div (:class "w-full h-fit font-mono text-white text-l
                            flex-initial") "Here is test text")
              ,(generate-paragraph-divs *page-content*))
          )
    (sidebar)
))

(defun generate-paragraph-divs (parsed-data &key (header-fn nil))
  "Generate div s-expressions for all paragraphs in parsed data.
   
   Parameters:
   - PARSED-DATA: The nested list structure from parse-text-file
   - HEADER-FN: Optional function called for each section header.
                Takes (heading-text section-index) and returns an s-expression
                to insert before that section's paragraphs.
   
   Returns a list of s-expressions (paragraphs + optional headers)."
  (let ((result nil))
    (loop for section in parsed-data
          for section-idx from 0
          do (let ((heading (first section))
                   (paragraphs (rest section)))
               
               ;; If header-fn provided, call it for this section
               (when header-fn
                 (let ((header-expr (funcall header-fn heading section-idx)))
                   (when header-expr
                     (push header-expr result))))
               
               ;; Generate div for each paragraph
               (dolist (paragraph paragraphs)
                 (push `(div (:class "w-full h-fit font-mono text-white text-l flex-initial")
                              ,paragraph)
                       result))))
    (nreverse result)))

(defun parse-text-file (filepath)
  "Read a text file and parse it into nested lists.
   Lines starting with # are headings that create new sections.
   Each section is a list with the heading (without #) as first element,
   followed by paragraph lines until the next heading."
  (with-open-file (stream filepath :direction :input)
    (let ((sections nil)
          (current-section nil))
      
      (loop for line = (read-line stream nil nil)
            while line
            do (let ((trimmed (string-trim '(#\Space #\Tab) line)))
                 (cond
                   ;; Empty lines are ignored
                   ((string= trimmed "")
                    nil)
                   
                   ;; Line starts with # - it's a heading
                   ((and (> (length trimmed) 0)
                         (char= (char trimmed 0) #\#))
                    ;; Save previous section if exists
                    (when current-section
                      (push (nreverse current-section) sections))
                    ;; Start new section with heading (strip # and whitespace)
                    (let ((heading (string-trim '(#\Space #\Tab) 
                                                (subseq trimmed 1))))
                      (setf current-section (list heading))))
                   
                   ;; Regular line - add to current section
                   (t
                    (if current-section
                        (push trimmed current-section)
                        ;; If no heading yet, create section with empty heading
                        (setf current-section (list "" trimmed)))))))
      
      ;; Don't forget the last section
      (when current-section
        (push (nreverse current-section) sections))
      
      (nreverse sections))))

(defun load-text-file-to-parameter (filepath parameter-name)
  "Load a text file and bind the parsed result to a defparameter.
   PARAMETER-NAME should be a symbol."
  (let ((parsed-data (parse-text-file filepath)))
    (eval `(defparameter ,parameter-name ',parsed-data))
    parsed-data))

(load-text-file-to-parameter "./content.txt" '*page-content*)
