;;;; Static Website Generator in SBCL
;;;; A simple but extensible system for generating HTML from S-expressions

(defpackage :website-generator
  (:use :cl)
  (:export #:build-page
           #:render-html
           #:define-component
           #:write-page-to-file
           #:generate-site
           #:raw-html
           #:make-raw-html
           #:page
           #:tailwind-cdn))

(in-package :website-generator)

;;; Core HTML generation functions

(defun escape-html (str)
  (with-output-to-string (out)
    (loop for char across str do
      (case char
        (#\< (write-string "&lt;" out))
        (#\> (write-string "&gt;" out))
        (#\& (write-string "&amp;" out))
        (#\" (write-string "&quot;" out))
        (#\' (write-string "&#39;" out))
        (t (write-char char out))))))

(defun attributes-to-string (attrs)
  "Convert attribute plist to HTML attribute string"
  (if attrs
    (with-output-to-string (out)
      (write-char #\Space out)
      (loop for (key value) on attrs by #'cddr do
        (format out "~a=\"~a\"" 
                (string-downcase (symbol-name key))
                (escape-html (princ-to-string value)))
        (when (cddr (member key attrs))
          (write-char #\Space out))))
    ""))

(defun render-element (tag attrs content)
  "Render a single HTML element"
  (let ((tag-name (string-downcase (symbol-name tag)))
        (attr-string (attributes-to-string attrs)))
    (if content
        (format nil "<~a~a>~{~a~}</~a>"
                tag-name attr-string content tag-name)
        (format nil "<~a~a></~a>" tag-name attr-string tag-name))))

(defstruct raw-html content)

;;; S-expression to HTML converter

(defgeneric render-html (element)
  (:documentation "Convert S-expression to HTML string"))

(defmethod render-html ((element string))
  "Render plain text (escaped)"
  (escape-html element))

(defmethod render-html ((element raw-html))
  "Render raw HTML without escaping"
  (raw-html-content element))

(defmethod render-html ((element number))
  "Render numbers as strings"
  (princ-to-string element))

(defmethod render-html ((element null))
  "Render nil as empty string"
  "")

(defmethod render-html ((element list))
  "Render S-expression lists as HTML"
  (cond
    ;; Empty list
    ((null element) "")
    
    ;; Check if it's a component (function call)
    ((and (symbolp (first element))
          (fboundp (first element))
          (get (first element) 'component))
     (render-html (apply (first element) (rest element))))
    
    ;; Standard HTML element: (tag attrs content...)
    ((symbolp (first element))
     (let ((tag (first element))
           (rest-elements (rest element)))
       (cond
         ;; No content: (tag)
         ((null rest-elements)
          (render-element tag nil nil))
         
         ;; First element is attribute plist: (tag (:attr val :attr2 val2) content...)
         ((and (listp (first rest-elements))
               (keywordp (first (first rest-elements))))
          (let ((attrs (first rest-elements))
                (content (mapcar #'render-html (rest rest-elements))))
            (render-element tag attrs content)))
         
         ;; No attributes, just content: (tag content...)
         (t
          (let ((content (mapcar #'render-html rest-elements)))
            (render-element tag nil content))))))
    
    ;; Multiple elements at top level
    (t (format nil "~{~a~}" (mapcar #'render-html element)))))

;;; Component system

(defmacro define-component (name args &body body)
  "Define a reusable component"
  `(progn
     (defun ,name ,args
       ,@body)
     (setf (get ',name 'component) t)
     ',name))

;;; Built-in components

(define-component doctype ()
  "HTML5 doctype"
  (make-raw-html :content "<!DOCTYPE html>"))

(define-component page (title head-content &rest body)
  "Complete HTML page with head and body"
  `((doctype)
    (html
      (head
        (meta (:charset "utf-8"))
        (meta (:name "viewport" :content "width=device-width, initial-scale=1.0"))
        (title ,title)
        ,@head-content)
      (body ,@body))))

(define-component tailwind-cdn ()
    '(script (:src "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4")))

(define-component code-block (language code)
  "Code block with syntax highlighting class"
  `(pre (code (:class ,(format nil "language-~a" language)) ,code)))

;;; Main build function

(defun build-page (&rest elements)
  "Build HTML page from S-expressions"
  (format nil "~{~a~}" (mapcar #'render-html elements)))

;;; File operations

(defun write-page-to-file (filename &rest elements)
  "Write generated HTML to file"
  (with-open-file (stream filename 
                          :direction :output 
                          :if-exists :supersede 
                          :if-does-not-exist :create)
    (write-string (apply #'build-page elements) stream))
  (format t "Generated: ~a~%" filename))

(defun generate-site (output-dir pages)
  "Generate multiple pages for a site"
  (ensure-directories-exist output-dir)
  (loop for (filename . content) in pages do
    (write-page-to-file 
      (merge-pathnames filename output-dir)
      content)))

;;; Example usage and demo

; (defun demo ()
;   "Demonstrate the website generator"
;
;   ;; Simple page
;   (format t "~%=== Simple Page ===~%")
;   (format t "~a~%~%" 
;     (build-page 
;       '(page "My Blog"
;         (h1 "Welcome to My Blog")
;         (p "This is a simple paragraph with " (strong "bold text") ".")
;         (ul 
;           (li "First item")
;           (li "Second item")
;           (li "Third item")))))
; )

;;; Advanced example: Blog generator

; (define-component blog-post (title date author &rest content)
;   "Blog post component with metadata"
;   `(article (:class "blog-post")
;     (header (:class "post-header")
;       (h2 (:class "post-title") ,title)
;       (div (:class "post-meta")
;         (time (:datetime ,date) ,date)
;         (span " by ")
;         (span (:class "author") ,author)))
;     (div (:class "post-content") ,@content)))
;
; (define-component blog-index (posts)
;   "Blog index page"
;   `(page "My Blog"
;     (header (h1 "My Blog"))
;     (main ,@(mapcar (lambda (post)
;                       `(blog-post ,@post))
;                     posts))))
;
; ;; Example blog data
; (defparameter *blog-posts*
;   '(("First Post" "2024-01-15" "John Doe"
;      (p "This is my first blog post!")
;      (p "I'm excited to share my thoughts."))
;     ("Learning Lisp" "2024-01-20" "John Doe"
;      (p "Today I learned about S-expressions.")
;      (code-block "lisp" "(+ 1 2 3)")
;      (p "Lisp is amazing!"))))

; (defun generate-blog ()
;   "Generate a complete blog"
;   (write-page-to-file 
;     "blog.html"
;     (blog-index *blog-posts*)))

;;; Run demo
;; Uncomment to run: (demo)
; (demo)
