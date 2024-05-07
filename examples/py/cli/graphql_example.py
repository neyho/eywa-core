import eywa
from datetime import datetime


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


query = """
mutation ($group:UserGroupInput!) {
  syncUserGroup (user_group: $group) {
    euuid
    active
    modified_on
    modified_by {
      name
    }
  }
}
"""


response = eywa.graphql({'query': query, 'variables': {
    "group": {
        "name": "Test Grup",
        "active": True
        # "modified_on": datetime(2000, 2, 3, 4, 5, 6).isoformat()
    }
    }}, 2)


print('Response:\n' + str(response))


query = """
{
  searchUserGroup {
    euuid
    active
    modified_on
    modified_by {
      name
    }
  }
}
"""

response = eywa.graphql({'query': query})
print('Response:\n' + str(response))
