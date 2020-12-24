(defsystem :hu.dwim.cldr-compiler
  :author "Attila Lendvai <attila@lendvai.name>"
  :version "0.1"
  :description "Compiles the CLDR xml files into CL files."
  :long-description "Parses and compiles the CLDR xml files to CL files. It shouldn't be of much interest to users, because its output is generated and checked in into the git repository by the maintainer whenever a newer CLDR version is used."
  :licence "MIT"
  :serial t
  :components ((:module :source
                        :components ((:file "package")
                                     (:file "util")
                                     (:file "parser")
                                     (:file "compiler"))))
  :depends-on (:alexandria
               :hu.dwim.common
               :hu.dwim.def
               :hu.dwim.flexml
               :hu.dwim.l10n
               :puri
               :iterate
               :metabang-bind
               :local-time
               :closer-mop))
