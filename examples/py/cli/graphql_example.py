import eywa


query = """
{
  searchPermission {
    euuid
    name
  }
}
"""


response = eywa.graphql({'query': query})


print('Response:\n' + str(response))
