(define-generic-mode 'nw-mode
  '("#" "\n")
  '("kind" "type" "atom" "operator" "rule" "strategy")
  '(
    ("\[[A-Z][A-Za-z\-]+\]" . 'font-lock-type-face)
    (":[a-z]+" . 'font-lock-function-name-face)
    ("--[a-z]+" . 'font-lock-function-name-face)
    (":[ ]*[A-Z][A-Za-z]+" . 'font-lock-type-face)
    ("->[ ]*[A-Z][A-Za-z]+" . 'font-lock-type-face)
    ("\*[ ]*[A-Z][A-Za-z]+" . 'font-lock-type-face)  
    ("[.][ ]*[A-Z][A-Za-z]+" . 'font-lock-type-face)  
    ("with" . 'font-lock-function-name-face)
    ("\?[A-Za-z]+" . 'font-lock-variable-name-face))
  '("\\.nw\\'")
  nil
  "Major mode for editing NoWork files")
