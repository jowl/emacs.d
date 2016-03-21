(prodigy-define-service
  :name "Elasticsearch"
  :command "elasticsearch"
  :ready-message "\] started$"
  :tags '(butterflies)
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "DynamoDB local"
  :command "java"
  :args '("-Djava.library.path=/usr/local/Cellar/dynamodb-local/2016-01-07_1.0/libexec/DynamodbLocal_lib" "-jar" "/usr/local/Cellar/dynamodb-local/2016-01-07_1.0/libexec/DynamoDBLocal.jar" "-inMemory")
  :ready-message "^$"
  :cwd "/usr/local/var/data/dynamodb-local"
  :tags '(butterflies)
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "MongoDB"
  :command "mongod"
  :args '("--config" "/usr/local/etc/mongod.conf")
  :cwd "/usr/local"
  :ready-message "waiting for connections on port"
  :tags '(hauptbahnhof einheit)
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Cassandra"
  :command "cassandra"
  :args '("-f")
  :ready-message "Listening for thrift clients..."
  :tags '(einheit)
  :kill-process-buffer-on-stop t)
