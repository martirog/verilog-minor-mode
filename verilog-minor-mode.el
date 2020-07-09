(defvar vminor-ctags-verilog-def
  '("--langdef=systemverilog"
    "--langmap=systemverilog:.v.vg.sv.svh.tv.vinc"
    "--regex-systemverilog=\"/^\\s*\\(\\b\\(static|local|virtual|protected\\)\\b\\)*\\s*\\bclass\\b\\s*\\(\\b\\w+\\b\\)/\\3/c,class/\""
    "--regex-systemverilog=\"/^\\s*\\(\\b\\(static|local|virtual|protected\\)\\b\\)*\\s*\\btask\\b\\s*\\(\\b\\(static|automatic\\)\\b\\)?\\s*\\(\\w+::\\)?\\s*\\(\\b\\w+\\b\\)/\\6/t,task/\""
    "--regex-systemverilog=\"/^\\s*\\(\\b\\(static|local|virtual|protected\\)\\b\\)*\\s*\\bfunction\\b\\s*\\(\\b\\(\\w+\\)\\b\\)?\\s*\\(\\w+::\\)?\\s*\\(\\b\\w+\\b\\)/\\6/f,function/\""
    "--regex-systemverilog=\"/^\\s*\\bmodule\\b\\s*\\(\\b\\w+\\b\\)/\\1/m,module/\""
    "--regex-systemverilog=\"/^\\s*\\bpackage\\b\\s*\\(\\b\\w+\\b\\)/\\1/P,package/\""
    "--regex-systemverilog=\"/^\\s*\\bprogram\\b\\s*\\(\\b\\w+\\b\\)/\\1/p,program/\""
    "--regex-systemverilog=\"/^\\s*\\binterface\\b\\s*\\(\\b\\w+\\b\\)/\\1/i,interface/\""
    "--regex-systemverilog=\"/^\\s*\\btypedef\\b\\s+.*\\s+\\(\\b\\w+\\b\\)\\s*;/\\1/e,typedef/\""
    "--regex-systemverilog=\"/^\\s*`define\\b\\s*\\(\\w+\\)/`\\1/d,define/\""
    "--regex-systemverilog=\"/}\\s*\\(\\b\\w+\\b\\)\\s*;/\\1/e,typedef/\""
    "--regex-systemverilog=\"/^\\s*\\(\\b\\(static|local|private|rand\\)\\b\\)*\\s*\\(\\b\\(shortint|int|longint\\)\\b\\)\\s*\\(\\bunsigned\\b\\)?\\(\\s*\\[.+\\]\\)*\\s*\\(\\b\\w+\\b\\)/\\7/v,variable/\""
    "--regex-systemverilog=\"/^\\s*\\(\\b\\(static|local|private|rand\\)\\b\\)*\\s*\\(\\b\\(byte|bit|logic|reg|integer|time\\)\\b\\)\\(\\s*\\[.+\\]\\)*\\s*\\(\\b\\w+\\b\\)/\\6/v,variable/\""
    "--regex-systemverilog=\"/^\\s*\\(\\b\\(static|local|private\\)\\b\\)*\\s*\\(\\b\\(real|shortreal|chandle|string|event\\)\\b\\)\\(\\s*\\[.+\\]\\)*\\s*\\(\\b\\w+\\b\\)/\\6/v,variable/\""
    "--regex-systemverilog=\"/\\(\\b\\(input|output|inout\\)\\b\\)?\\s*\\(\\[.+\\]\\)*\\s*\\(\\b\\(wire|reg|logic\\)\\b\\)\\s*\\(\\[.+\\]\\)*\\s*\\(#\\(\\(.+\\)|\\S+\\)\\)\\)?\\s*\\(\\b\\w+\\b\\)/\\9/v,variable/\""
    "--regex-systemverilog=\"/\\(\\b\\(parameter|localparam\\)\\b\\).+\\(\\b\\w+\\b\\)\\s*=/\\3/a,parameter/\""
    "--systemverilog-kinds=\"+ctfmpied\"")
  "define how ctags should find system verilog tags")

(defvar vminor-path-to-repos nil
  "list of cons: repos to search for systemverilog files . exclude list")

(setq vminor-path-to-repos '(("/A" . ("a" "aa")) ("/B" . ("b" "bb"))))

(defvar vminor-file-extention
  '("v" "sv" "vh" "svh")
  "file extentions to use in the search for files to tag")

(defvar vminor-tag-path "/home/martin/" ;nil
  "path to puth the TAGS file")

(defvar vminor-tag-file-name "vminor_TAGS"
  "name of the TAGS file")

(defun vminor-regen-tags()
  (interactive) ; make this ask for files and paths
  (let ((tag-file (concat vminor-tag-path vminor-tag-file-name))
        (repos vminor-path-to-repos))
    ; delete excisting TAGS file
    (if (file-exists-p tag-file)
        (progn
          (message "deleting file: %s" tag-file)
          (delete-file tag-file)))
    ; iterate over repos
    (while repos
      (let ((cmd "find")
            (exclutions (cdr (car repos)))
            (repo (car (car repos)))
            (ctags-switches vminor-ctags-verilog-def)
            (extentions vminor-file-extention)
            (first t))
        (setq cmd (concat cmd " " repo))
        ; iterate over paths in the repo to ignore
        (while exclutions
          (if (null first)
              (setq cmd (concat cmd " -or")))
          (setq cmd (concat cmd " -path \"*" (car exclutions) "*\" -prune"))
          (message cmd)
          (setq exclutions (cdr exclutions))
          (setq first nil))
        ; iterate over file extentions to search in
        (while extentions
          (if (null first)
              (setq cmd (concat cmd " -or")))
          (setq cmd (concat cmd " -name \"*" (car extentions) "*\""))
          (message cmd)
          (setq extentions (cdr extentions))
          (setq first nil))
        ; add ctags command
        (setq cmd (concat cmd " | xargs ctags -a -e"))
        (while ctags-switches
          (setq cmd (concat cmd " " (car ctags-switches)))
          (message cmd)
          (setq ctags-switches (cdr ctags-switches)))
        (setq cmd (concat cmd " -o " tag-file))
        (message cmd)
        (shell-command cmd)
      (setq repos (cdr repos))))))

(define-minor-mode verilog-minor-mode
  "Get your foos in the right places."
  :lighter " vmin")

(provide 'verilog-minor-mode)

  ;:keymap (let ((map (make-sparse-keymap)))
  ;          (define-key map (kbd "C-c f") 'insert-foo)
  ;          map))
