(load "website-builder.lisp")

(in-package :cl-user)
(defpackage :pHish-site
  (:use :cl :website-generator))

(in-package :pHish-site)

(define-component sidebar ()
  "Sidebar with cool opening animation"
  `(div (:class "size-[3rem] absolute top-[3rem] right-[3rem]")
     (div (:class "relative")
         (button (:type "button"
                  :class "bg-blue size-[3rem]
                  rounded-xl absolute cursor-pointer"
                  :onclick "press_menu()"))
         (div (:class "border-blue size-[3rem] 
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
    (sidebar)
))
