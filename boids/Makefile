SRC = src
CLASSES = classes

all:	$(CLASSES)
	scalac -d $(CLASSES) $(SRC)/boids/*.scala

run:
	scala -cp $(CLASSES) boids.BoidRunner

$(CLASSES):
	mkdir -p $(CLASSES)

clean:
	rm -rf $(CLASSES)
