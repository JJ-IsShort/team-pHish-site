(load "website-builder.lisp")

(in-package :cl-user)
(defpackage :pHish-site
  (:use :cl :website-generator))

(in-package :pHish-site)

(define-component sidebar ()
  "Sidebar with cool opening animation"
  `(div (:class "size-[3rem] absolute top-[3rem] right-[3rem]")
     (div (:class "relative")
         (div (:class "border-blue-dark size-[3rem] 
               rounded-xl cursor-pointer border-8 absolute"))
         (div (:class "border-blue-light size-[3rem]
               rounded-xl border-8 absolute top-[-0.1rem] right-[0.1rem]"))))
)

(write-page-to-file "index.html"
  '(page "Example"
    ((script (:src "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"))
     (style (:type "text/tailwindcss") "@theme {
            --color-*: initial;
            --color-blue-dark: #023047;
            --color-blue: #219ebc;
            --color-blue-light: #8ecae6;
            --color-white: #fff;
            --color-yellow: #FFB703;
            --color-yellow-dark: #FB8500;
            }"))
    (div (:class "bg-yellow-dark w-screen h-screen" ))
    (sidebar)
))
