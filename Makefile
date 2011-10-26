CLASS_DIR = classes
CLASSPATH = .:/usr/local/lib/scalatest.jar

test:
	mkdir -p $(CLASS_DIR)
	scalac -d $(CLASS_DIR) -cp $(CLASSPATH) Base64*.scala
	scala -cp $(CLASS_DIR):$(CLASSPATH) org.scalatest.tools.Runner -o -s jimm.util.Base64Suite

clean:
	rm -rf $(CLASS_DIR)
