files = lytspel test_lytspel.py
mypy = mypy --ignore-missing-imports

all: lint typecheck test

lint:
	pylint3 $(files)

typecheck:
	$(mypy) $(files)

test:
	pytest-3
