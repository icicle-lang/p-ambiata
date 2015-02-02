hydra
=====

```
Hydra: the multi-headed daemon. Hydra is responsible for scheduling and
co-ordinating tasks via precondition, time or dependency in a distributed
environment.
```

### Concepts

#### `Task`

A task is something to be run. It would be normally some script to run using
a set of (potentially computed per run) arguments. Tasks have some trigger
and set of preconditions.

#### `Run`

A unique execution for a task. A single occurrence should only be completed
successfully once.

#### `Attempt`

A `Run` attempt. Normally a `Run` would only have one attempt, unless it fails
and there is an explicit request to re-try, which would mean a `Run` may have
more than one `Attempt`.

#### `Trigger`

Some (normally time based) trigger which will to attempt to schedule a `Run` of
a `Task` if the pre-conditions are met.

#### `Precondition`

A set of pre-conditions that must hold true for a job to actualy run.


### Stages of a `Task -> Run`

 - Should I check if should run? - fast, easy to determine, regular interval.
 - Should I run? - potentially slow, not evenly distributed, can fail.
 - Run - long running, blocking.

### Development

Setting up database access to re-use hydra-dev:

```
ssh -f -N -L 5432:hydra-dev.cpd0t2ayi9o9.ap-southeast-2.rds.amazonaws.com:5432 ci
export HYDRA_DB="dbname=hydra host=localhost user=hydra password=7bdea8e6a1905b385c8eb43564496d7e2ab88429 port=5432"
export HYDRA_POOL=10
```


Setting up local access:
```
dropdb hydra
createuser -S -d -R hydra
createdb hydra
echo "alter user hydra with SUPERUSER;" | psql hydra
echo "alter user hydra with encrypted password 'hydra';" | psql hydra
echo "grant all privileges on database hydra to hydra;" | psql hydra
export HYDRA_DB="dbname=hydra host=localhost user=hydra password=hydra port=5432"
export HYDRA_POOL=10
```

Setting up local access for demo:
```
dropdb hydra_demo
createuser -S -d -R hydra
createdb hydra_demo
echo "alter user hydra with SUPERUSER;" | psql hydra
echo "alter user hydra with encrypted password 'hydra';" | psql hydra
echo "grant all privileges on database hydra_demo to hydra;" | psql hydra
export HYDRA_DB="dbname=hydra_demo host=localhost user=hydra password=hydra port=5432"
export HYDRA_POOL=10
```
