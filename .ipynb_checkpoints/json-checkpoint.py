
# importing json file 

import os

# get working directory
os.getcwd()

import json
import pd






json_documents = []
with open('/Users/alexandergamerdinger/Downloads/file.jsonl', 'r') as fh:
  for line in fh:
    json_documents.append( json.loads(line) )



df = open('/Users/alexandergamerdinger/Downloads/file.jsonl', "r")

da = json.loads(df.read())
