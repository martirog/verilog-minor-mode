;; -*- lexical-binding: t -*-

(require 'etags-wrapper)
(require 'project-wrapper)
(defvar vminor-ctags-verilog-def
  '("--language=none"
    "--regex=\"/^[ \\t]*\\(extern\\|static\\|local\\|virtual\\|protected\\|interface\\)*[ \\t]*class[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\2/\""
    "--regex=\"/^[ \\t]*\\(extern\\|static\\|local\\|virtual\\|protected\\)*[ \\t]*task[ \\t]*\\(static\\|automatic\\)?[ \\t]*\\([0-9a-zA-Z\\$_]+::\\)?\\([0-9a-zA-Z\\$_]+\\).*/\\4/\""
    "--regex=\"/^[ \\t]*\\(import \\(.DPI-C[ \\t]*\\(\\.*[ \\t]*=[ \\t]*\\)?\\).\\)?[ \\t]*\\(context\\|extern\\|static\\|local\\|virtual\\|protected\\)*[ \\t]*function[ \\t]*\\(static\\|automatic\\)?[ \\t]*\\([0-9a-zA-Z\\$_]+\\)?[ \\t]*\\(\\[[.*]*\\]\\)?[ \\t]*\\([0-9a-zA-Z\\$_]+::\\)?\\([0-9a-zA-Z\\$_]+\\).*/\\9/\""
    "--regex=\"/^[ \\t]*module[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
    "--regex=\"/^[ \\t]*package[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
    "--regex=\"/^[ \\t]*program[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
    "--regex=\"/^[ \\t]*interface[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\1/\""
    "--regex=\"/^[ \\t]*typedef[ \\t]+.*[ \\t]+\\([0-9a-zA-Z\\$_]+\\)[ \\t]*\\(\\[.*\\]+\\)?[ \\t]*;/\\1/\""
    "--regex=\"/^[ \\t]*\\`define[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\`\\1/\""
    "--regex=\"/^.*}[ \\t]*\\([0-9a-zA-Z\\$_]+\\)[ \\t]*;/\\1/\""
    ;"--regex=\"/^.*)[ \\t]*\\([0-9a-zA-Z\\$_]+\\)[ \\t]*;/\\1/\""
    ;"--regex=\"/^[ \\t]*\\(static\\|local\\|private\\|rand\\)*[ \\t]*\\(shortint\\|int\\|longint\\)[ \\t]*unsigned?\\([ \\t]*[.+]\\)*[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\4/\""
    ;"--regex=\"/^[ \\t]*\\(static\\|local\\|private\\|rand\\)*[ \\t]*\\(byte\\|bit\\|logic\\|reg\\|integer\\|time\\)\\([ \\t]*[.+]\\)*[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\4/\""
    ;"--regex=\"/^[ \\t]*\\(static\\|local\\|private\\)*[ \\t]*\\(real\\|shortreal\\|chandle\\|string\\|event\\)\\([ \\t]*[.+]\\)*[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\4/\""
    ;"--regex=\"/^[ \\t]*\\(input\\|output\\|inout\\)?[ \\t]*\\([.+]\\)*[ \\t]*\\(wire\\|reg\\|logic\\)[ \\t]*\\([.+]\\)*[ \\t]*\\(#\\(\\(.+\\)\\|[ \\t]+\\)\\)?[ \\t]*\\([0-9a-zA-Z\\$_]+\\)/\\8/\""
    "--regex=\"/^[ \\t]*\\(parameter\\|localparam\\).*[ \\t]+\\([0-9a-zA-Z\\$_]+\\)[ \\t]*=.*;/\\2/\"")
  "define how ctags should find system verilog tags")

(defvar vminor-path-to-repos nil
  "list of cons: repos to search for systemverilog files . exclude list")

(defvar vminor-file-extention
  '("v" "sv" "vh" "svh")
  "file extentions to use in the search for files to tag")

(defvar vminor-tag-path (concat (getenv "HOME") "/") ;nil
  "path to puth the TAGS file")

(defvar vminor-tag-file-post-fix "vminor_TAGS"
  "name of the TAGS file")

(defvar vminor-use-vc-root-for-tags t
  "you only want tags for the current repo and that repo uses GIT")

(defvar vminor-sv-key-words
  '("begin" "end"
    "module" "endmodule"
    "initial" "always"
    "function" "endfunction"
    "task" "endtask"
    "package" "endpackage"
    "class" "endclass"
    "extends" "implements"
    "static" "automatic"
    "foreach" "break"
    "semaphore"
    "covergroup" "coverpoint" "cross"
    "constraint"
    "push_back" "push_front" "pop_back" "pop_front"
    "$display" "$sformatf")
  "System verilog keywords and functions")

(defvar vminor-common-uvm
  '("uvm_object" "`uvm_object_utils"
    "uvm_component" "`uvm_component_utils")
  "System verilog keywords and functions")

; copied from https://www.emacswiki.org/emacs/HippieExpand
; this need to be updated to allow for dollar sign($) or back tick in front of the word
(defun vminor--tag-beg ()
  "find the start of the start of the system verilog expression. this only looks at words for now(so I lied in the first sentece)"
  (let ((p
         (save-excursion
           (backward-word 1)
           (if (memq (preceding-char) '(?$ ?`))
               (backward-char))
           (point))))
    p))

; copied from https://www.emacswiki.org/emacs/HippieExpand
(defun vminor--tags-complete-tag (string predicate what)
  "find compleations from tags table"
  (save-excursion
    (if (etags-wrapper-check-for-tags-table)
      (if (eq what t)
          (all-completions string (tags-completion-table) predicate)
        (try-completion string (tags-completion-table) predicate))
      nil)))

(defun vminor--try-expand-tag (old)
  "try to find compleations in tags"
  (vminor--find-expand-tag old 'vminor--tags-complete-tag))

(defun vminor--try-expand-sv (old)
  "try to find compleation from the system verilog spec"
  (vminor--find-expand-tag old vminor-sv-key-words))

(defun vminor--try-expand-common-uvm (old)
  "try to find compleation from the most commenly used UVM structures"
  (vminor--find-expand-tag old vminor-common-uvm))

(defun vminor--find-expand-tag (old what)
  "find tags from a function or a list this is made to fit in make-hippie-expand-function"
  (unless  old
    (he-init-string (vminor--tag-beg) (point))
    (setq tags-he-expand-list (sort
                          (all-completions he-search-string what) 'string-lessp)))
  (while (and tags-he-expand-list
              (he-string-member (car tags-he-expand-list) he-tried-table))
    (setq tags-he-expand-list (cdr tags-he-expand-list)))
  (if (null tags-he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (car tags-he-expand-list))
    (setq tags-he-expand-list (cdr tags-he-expand-list))
    t))


(require 'icrib-buffer-and-tag-compleation nil t)
(if (featurep 'icrib-buffer-and-tag-compleation)
    (defun vminor--expand-abbrev (dummy)
      (let ((init-string (buffer-substring-no-properties (vminor--tag-beg) (point))))
        (icrib-buffer-and-tag-compleation init-string '("verilog-mode") nil vminor-sv-key-words)))

                                        ; compose the compleation order
  (defalias 'vminor--expand-abbrev (make-hippie-expand-function
                                   '(try-expand-dabbrev
                                     vminor--try-expand-sv
                                     vminor--try-expand-common-uvm
                                     try-expand-dabbrev-visible
                                     vminor--try-expand-tag))))



(defun vminor-verilog-tab ()
  "extend the verilog mode tab so that if the verilog-mode tab has no affect and we are at the end of a word we use the vminor--expand-abbrev function"
  (interactive)
  (let ((boi-point
           (save-excursion
             (back-to-indentation)
             (point))))
    (electric-verilog-tab)
    (if (and
         (save-excursion
          (back-to-indentation)
          (= (point) boi-point))
         (looking-at "\\>"))
        (vminor--expand-abbrev nil)
      (setq he-num -1))))

(defun vminor--setup-etags-wrapper()
  (setq-local etags-wrapper-switche-def vminor-ctags-verilog-def)

  (if (require 'project nil t)
      (progn
        (project-wrapper-initialize (project-root (project-current)))
        (setq-local etags-wrapper-path-to-repos (project-wrapper-etags-paths-to-repos))
        (when (and (null etags-wrapper-path-to-repos) vminor-path-to-repos)
          (setq-local etags-wrapper-path-to-repos vminor-path-to-repos)))
    (setq-local etags-wrapper-path-to-repos vminor-path-to-repos))

  (setq-local etags-wrapper-file-extention vminor-file-extention)
  (setq-local etags-wrapper-tag-path vminor-tag-path)
  (setq-local etags-wrapper-tag-file-post-fix vminor-tag-file-post-fix)
  (setq-local etags-wrapper-use-vc-root-for-tags vminor-use-vc-root-for-tags)
  (setq-local tags-table-list
              (etags-wrapper-generate-tags-list etags-wrapper-path-to-repos)))

(require 'verilog-mode)
(require 'hideshow)
(define-minor-mode verilog-minor-mode
  "set up verilog minor mode"
  :lighter " vmin"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "\t" 'vminor-verilog-tab)
            (define-key map (kbd "C-c a") 'hs-toggle-hiding)
            map)
  (vminor--setup-etags-wrapper)
  (add-hook 'verilog-mode-hook 'hs-minor-mode)
  (add-to-list 'hs-special-modes-alist (list 'verilog-mode (list verilog-beg-block-re-ordered 0) "\\<end\\>" nil 'verilog-forward-sexp-function))
  (flyspell-prog-mode))

(provide 'verilog-minor-mode)
