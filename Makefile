all: init messageboard 

requirements:
	sh requirements.sh || (echo "Requirements not met. Exiting..."; exit 1)

messageboard: journal.cob
	cobc -x journal.cob -o journal

init:
	touch ./JRN

clean:
	rm -rf journal
