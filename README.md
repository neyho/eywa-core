[![Clojars Project](https://img.shields.io/clojars/v/org.neyho/eywa-core.svg)](https://clojars.org/org.neyho/eywa-core)
[![Clojars Project](https://img.shields.io/clojars/v/org.neyho/eywa-core-frontend.svg)](https://clojars.org/org.neyho/eywa-core-frontend)


#### EYWA Core
EYWA Core is consisted of IAM and Dataset modeling. Dataset modeling is heart of this project and
its goal is to enable users to model their data and deploy modeled data painlessly to underlaying DB
engine/service. With this approach data models are transportable (from DB to DB, from project to project),
reliable and provide common ground for extending application features.

Identity Access Management is necessary to enable secure and consistent way to control modeling, deployment,
and usage of deployed models so that only the right people/service/script can do the right thing. Therefore
IAM is indivisable part of Dataset modeling.


#### Installation
##### Windows

```ps1
Invoke-WebRequest -Uri "https://s3.eu-central-1.amazonaws.com/eywa.public/eywa_cli/install_eywa_cli.ps1" -OutFile eywa_cli_install.ps1
./eywa_cli_install.ps1
rm eywa_cli_install.ps1
```
Add ```%USERPROFILE%\.eywa\bin``` to PATH environment variable


##### Linux and MacOs
```bash
curl -s https://s3.eu-central-1.amazonaws.com/eywa.public/eywa_cli/install_eywa_cli.sh | bash
```


#### Quickstart
Currently supported database is Postgres, although support for MySQL, SQLite and others is on roadmap. 

Requirement is to have Postgres, so if you don't have installation i recommend using docker to download Postgres
image and afterwards run:
```
docker run --name EYWA_DEV -p 5432:5432 -e POSTGRES_PASSWORD=password postgres
```

This will spin up container with port forwarding so that you can connect to database on localhost using
postgres user.

Following steps are enough to initialize EYWA.


EYWA will try to setup environment based on configuration file or environment variables. EYWA requires
credentials and DB endpoint data so it can connect to Postgres DB. Configuration file can be generated by
running

```
eywa core gen-config -j # for JSON configuration
eywa core gen-config -e # for EDN configuration
```

Both configuration formats are valid. For more information about how EYWA prioritizes environment setup
run
```
eywa core gen-config
```

Since configuration file will contain confidential data in plaintext it is recommended to add
```
eywa.json
eywa.edn
```
to _.gitignore_ file in your project.


To see what options are available to manage configuration file use:
```
eywa core config-schema
```
Lets get back on track, so we have running docker container and eywa cli application installed. As well
as local environment configured in configuration file and set as default environment for eywa cli.

Now we need to install EYWA Core jar file. Run
```
eywa core -l

And output should look something like this
❯ eywa core -l          
List of available versions. '*' installed, '-' not installed

[-] 0.1.4
[-] 0.1.3

```
To install some version run
```
eywa core -s 0.2.0
```
Ok EYWA Core server is installed, now we need to initialize EYWA IAM and Datacraft. So we should run:
```
eywa core init
```
If above command didn't throw any error that implies that initialization were successfull and DB is initialized.
So everything is ready to start EYWA server, except there is no user that can login to EYWA.
```
eywa core super -s admin
```
Will prompt for password for admin password, and when supplied admin user will be created with target password.
Now run
```
eywa core start
```
And navigate to **https://my.eywaonline.com/eywa/** and login screen should be waiting for you. Use username and
password from previous step to login.

To track what is happening open log file at location __~/.eywa/logs/dev.log__


If something went wrong or EYWA Core server isn't running as supposed to, run
```
eywa core doctor
```



#### CLI
__eywa__ cli client can connect to EYWA server by running
```
eywa connect http://localhost:8080
```
You will have to complete device code flow for target user and
after successfull connection client will use provided tokens to
interact with EYWA server.

Client also supports executing scripts through
```
eywa run -c "py example.py"
```
This will encapsulate running script process and by using one of
supported client libraries script can interact with connected EYWA
server with authorized EYWA user. The part above

For now we support:
 * Python - [eywa-client](https://pypi.org/project/eywa-client/)
 ``` pip install eywa-client```
 * Javascript - [eywa-client](https://www.npmjs.com/package/eywa-client)
 ```npm i eywa-client```

EYWA can be controlled through environment variables as well. To see
how, run:

```
eywa core -h
```

[Docs](https://www.eywaonline.com/docs/graphql/graphql-main)



#### Features
- [x] - GraphQL Sync Mutation
- [x] - GraphQL Stack Mutation
- [x] - GraphQL Purge Mutation
- [x] - GraphQL Delete Mutation
- [x] - GraphQL Sync/Stack List mutation for batch insertion/update 
- [x] - GraphQL Search Query
- [x] - GraphQL Get Query
- [x] - GraphQL SearchTree Query for recursive relations
- [x] - Aggregate fields for Query operations (count, avg, max, min, sum)
- [x] - Hash attribute type
- [x] - Encryption attribute type
- [ ] - Currency attribute type
- [x] - Dynamic GraphQL schema hooks
- [x] - Dynamic GraphQL schema resolvers for mutations, queries and subscriptions


#### Supported DB
- [x] - PostgreSQL implementation
- [ ] - MySQL implementation
- [ ] - SQLite implementation 
- [ ] - CochroachDB implementation 
- [ ] - XTDB implementation 


#### OAuth & OIDC
- [x] - Authorization Code flow
- [x] - Device Code flow
- [ ] - MultiFactor authentication
- [ ] - Passkey authentication
- [x] - IAM Role Access Management
- [x] - IAM Client registration
- [x] - IAM API permission control
- [ ] - External Identity Providers - module for easy onboarding 
- [ ] - Google Identity Provider
- [ ] - Github Identity Provider
- [ ] - AuthO Identity Provider
- [ ] - Amazon Identity Provider
- [ ] - AD/LDAP Identity Provider
- [ ] - Persistent sessions
- [ ] - Persistent tokens
- [ ] - Persistent JWKS certificate pairs
- [ ] - JWKS key rotation


#### Misc
- [ ] - Explore module - dynamic visualization
- [ ] - Documentation
- [ ] - Use cases / examples
