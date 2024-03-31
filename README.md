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



#### Setup
Check out [setup.md](./doc/setup.md)


#### Quickstart
First run your setup so you are ready to acctually run project. Currently supported database is Postgres,
although support for MySQL, SQLite and others is on roadmap. 

Requirement is to have Postgres, so if you don't have installation i recommend using docker to download Postgres
image and afterwards run:
```
docker run --name EYWA_DEV -p 5432:5432 -e POSTGRES_PASSWORD=password postgres
```

This will spin up container with port forwarding so that you can connect to database on localhost using
postgres user.

For development purposes you can use __src/dev/eywa.properties__ file to store variables for DB connection.
In production it is recommended to use environment variables following 12 Factor App pattern.

Copy __src/dev/eywa.properties.example__ to __src/dev/eywa.properties__ and modify it if necessary. That should
do it. If you are using Docker and above command than passwords should match.


Next run:
```
clj -A:dev
```
to open REPL, or if you wan't to use nREPL use:
```
clj -A:dev:cider
```

After opening REPL, namespace should be user, and src/dev/user.clj file is loaded so we can use functions
defined in that namespace.

 * To setup DB evaluate ```(setup)``` in REPL and it will create tables in DB for IAM and Datasets.
 * To start EYWA Server evaluate ```(-main)``` in REPL
 * There are still no users in EYWA. To add users evaluate
 ``` clojure
 (do
    (neyho.eywa.administration/setup
      {:users
       [{:name "test" :password "test" :active true
         :roles [neyho.eywa.data/*ROOT*]}]
       :roles [neyho.eywa.data/*ROOT*]})
    (neyho.eywa.dataset/load-role-schema))
 ```

Navigate to **http://localhost:8080/eywa/** and login screen should be waiting for you. Use username and
password from previous step to login.


To destroy EYWA instance evaluate ```(tear-down)``` in REPL


To track what is happening open log file at location __~/.eywa/logs/dev.log__


#### Dataset sources
For working on datasets and testing reconsider following datasets:
 * https://musicbrainz.org/
 * https://developer.imdb.com/non-commercial-datasets/
 * https://archive.org/details/stackexchange
 * Python - [Faker](https://faker.readthedocs.io/en/master/)
