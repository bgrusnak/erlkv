# erlkv
A test project - HTTP REST key-value storage on Erlang

Usage
--------

```
List items

http://localhost:8080/
-> will return the list of all keys

Get item

http://localhost:8080/KEY
-> will return the value of KEY

Add item

POST to 
http://localhost:8080

with values key, value, ttl (non-mandatory)

Delete item

DELETE to
http://localhost:8080/KEY


```
