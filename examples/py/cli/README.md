### Command Line Interface - EYWA
This is simple demonstration on how to interact with EYWA through command line interface.
First install eywa as described in project root folder. Prerequisite is that you have installed
and initialized EYWA datasets.
```
# Configure your local development environment for EYWA CLI
eywa env -s local
URL (https://example.eywaonline.com) : http://localhost:8080
User: admin
Password: admin
```
After configuring local environemnt ensure that EYWA Core is runnning, by navigating to
__http://localhost:8080/eywa__

If frontend is available we can continue. If not you should go one more time through README.md
in root directory of this project.


Install EYWA CLI dependencies
```
pip install eywa-client
```

### Running commands
EYWA CLI will use previously configured environment to connect to EYWA server instance and
interact with it through eywa-client interface. Running following commands will return list
of permissions that are available at target environment. Ofcourse you can use all operations
available through usual GraphQL interface.

```
eywa run -e local -c "python graphql_example.py"
```


We use this approach at Neyho to develop robots for our customers that we later onboard to
EYWA Robotics module that provides control, reports and multisite scaling.

