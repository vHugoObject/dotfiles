EMACS ?= emacs
SELECTOR ?= (tag :org)
test: ## test-runner for all org files
test:
	$(EMACS) --batch -L . \
		 -l init-files/org-functions-tests.el \
		 --eval "(ert-run-tests-batch-and-exit '$(SELECTOR))"; \
