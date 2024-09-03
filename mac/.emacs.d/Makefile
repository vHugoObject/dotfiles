EMACS ?= emacs
SELECTOR ?= (tag :org)
test: ## test-runner for all org files
test:
	$(EMACS) --batch -L . \
		 -l init-files/org-table-custom-functions-tests.el \
		 --eval "(ert-run-tests-batch-and-exit '$(SELECTOR))"; \
