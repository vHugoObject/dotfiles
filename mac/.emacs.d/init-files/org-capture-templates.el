(setq org-capture-templates
	     '(("c" "Les calories" table-line (file+headline "~/org/la-nourriture.org" "Les calories")
		"|%T|%^{PROMPT|500}|")
	       ("s" "La sucre" table-line (file+headline "~/org/la-nourriture.org" "La sucre")
		"|%T|%^{PROMPT}|")
	       ("a" "Les dépenses" table-line (file+headline "~/org/les-sous.org" "Les dépenses")
		  "|%T|%^{PROMPT}|")
	       ))

(provide 'org-capture-templates)
