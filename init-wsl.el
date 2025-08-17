;; mozc
(use-package mozc
  :ensure t
  :demand t
  :bind (("C-\\" . toggle-input-method))
  :config
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'echo-area))

(set-face-attribute 'default nil
                    :family "IPAexGothic"
                    :height 280)
(set-fontset-font "fontset-default" '(#x3000 . #x303F) "IPAexGothic" nil 'prepend)
(set-fontset-font "fontset-default" 'japanese-jisx0208 "IPAexGothic" nil 'prepend)
(set-fontset-font "fontset-default" 'japanese-jisx0212 "IPAexGothic" nil 'prepend)
(set-fontset-font "fontset-default" 'japanese-jisx0213-1 "IPAexGothic" nil 'prepend)
(set-fontset-font "fontset-default" 'japanese-jisx0213-2 "IPAexGothic" nil 'prepend)
(set-fontset-font "fontset-default" 'han "IPAexGothic" nil 'prepend)
(set-fontset-font "fontset-default" 'kana "IPAexGothic" nil 'prepend)
