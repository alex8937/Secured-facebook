# Secured-facebook
Graph database to store social network information supporting high-concurrent secured data insertion, deletion, modification and searching using AKKA framework.
# Highlights:
High-concurrent REST API interface for this graph database using Spray framework as a back-end server for social network application.

Security mechanism for the system using AES-256, DSA and RSA-1024 encryption that supports end-to-end encryption so the encrypted information on server and can only be decrypted on client side 

Client simulator for testing the performance of the system. The simulator successfully simulate over 10,000 users simultaneously access the REST server with different frequency of posting texts, posting photos, adding friends, refreshing status. 
