.PHONY: env book publish clean preview help

# Default target
help:
	@echo "Available targets:"
	@echo "  make env      - Setup renv and install R packages"
	@echo "  make book     - Render the Quarto book"
	@echo "  make preview  - Preview the book locally"
	@echo "  make publish  - Pull, commit all changes, and push"
	@echo "  make clean    - Remove rendered output"

# Setup renv environment and install packages
env:
	@echo "Setting up renv environment..."
	Rscript -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv')"
	Rscript -e "renv::restore(prompt = FALSE)"
	@echo "Done! All packages installed."

# Render the Quarto book
book:
	@echo "Rendering Quarto book..."
	quarto render
	@echo "Done! Output in _book/"

# Preview the book locally
preview:
	quarto preview

# Publish: pull, add, commit, push
publish:
	@echo "Publishing changes..."
	git pull --rebase
	git add .
	@read -p "Commit message: " msg; \
	git commit -m "$$msg" || echo "Nothing to commit"
	git push
	@echo "Done!"

# Clean rendered output
clean:
	@echo "Cleaning up..."
	rm -rf _book
	rm -rf _freeze
	@echo "Done!"
