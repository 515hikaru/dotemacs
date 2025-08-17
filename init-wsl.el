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
