# Taurus

Taurus is an experimental resolver for RBAC-based permissions systems.
It operates on a table of `policies` and checks incoming authorisation
requests against known policies in that table.

## Policies and Requests

Policies are essentially tuples containing the id of the role which this
policy concerns, the action the role is permitted to perform, and the
resource on which that action may be performed:

    (role, action, resource)

Taurus specifies actions and resources with the same syntax: parts
are separated with `:`, and wildcards (`*`) can match any part.
Additionally, because resources often need to be specified (e.g. you
may want to give permission to a specific `project`, not just any
`project`), resources have the specification syntax which looks like
`type/id`. For example, the following policy asserts that the role with
ID `42` has access to configure projects for all projects within the
organisation with ID `27`:

    (42, project:configure, org/27:project/*)

Taurus has no semantic understanding of the structure of actions or
resources, it just knows how to match these strings together.

Authorisation requests are similar to policies, except that wildcards
are not permitted: everything has to be specified. The reason for this
is that a request should be to apply a specific action to a specific
resource.

Taurus matches requests against policies to check whether a request
should be allowed or denied. The following policy / request pair will
result in permission being granted:

    Policy = (42, project:*, org/27:project/*)
    Request = (42, project:configure, org/27:project/12)

Whereas this pair will result in permission being denied:

    Policy = (42, project:*, org/27:project/*)
    Request = (42, project:create, org/27)

This is because the resource requested isn't a subset of that defined in
the policy.

_NB: the syntax for policies and requests shown here isn't actual code, just a
representation for the purposes of documentation._

## Usage

### Setup

Taurus expects to have access to a PostgreSQL server over the network.
It requires a table to exist called `policies`, and for that table to
have the following minimal schema:

    CREATE TABLE policies (
      role_id int not null,
      action text not null,
      resource text not null
    );

### Configuration

Taurus is configured via environment variables as follows:

* `TAURUS_SOURCE_ADDR`: the address of the PostgreSQL server.
* `TAURUS_SOURCE_PORT`: the port of the PostgreSQL server.
* `TAURUS_SOURCE_USER`: the PostgreSQL user.
* `TAURUS_SOURCE_PASS`: the PostgreSQL user's password.
* `TAURUS_SOURCE_DB`: the PostgreSQL database to use.

If running locally, the `./local-postgresql.sh` script will output the
correct environment variables after spinning up a local instance.

### Interacting over HTTP

#### Making an authorisation request

    GET http://taurus:1337/request/:roleId/:action/:resource

Parameters:
* `roleId` the ID of the role making the request.
* `action` the (urlencoded) action string (e.g. `org:CreateProject`).
* `resource` the (urlencoded) resource string (e.g. `org/42`).

This route can return data either in plaintext or json formats, you just
need to specify the right mimetype (`text/plain` or `application/json`)
in the `Accept` header. The response with contain the text "allow" or
"deny".

Examples:

    curl -X GET -H "Accept: text/plain" \
      "http://taurus:1337/request/42/org%3ACreateProject/org%2F42"
    // Response:
    200 OK
    Allow

    curl -X GET -H "Accept: application/json" \
      "http://taurus:1337/request/42/org%3ACreateProject/org%2F42"
    // Response:
    200 OK
    { "response": "allow" }

### As a servless application

 TODO
