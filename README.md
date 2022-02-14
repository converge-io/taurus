# Taurus

This is an experimental authorisation resolver package which takes
auth policies of the form `(subject, verb, object)` and a request of a
similar form, and returns a boolean `accept` or `deny` status.

## Roles and Actors

This resolver is based on RBAC (role-based access control) and, as such,
any actor wishing to gain permission must have a set of roles defined
for it. Since the types of actors and the storage used to get them can
vary from application to application, the resolver expects to receive a
list of roles rather than a specific actor as part of the request.

## Policies and Requests

Policies are of the form `(subject, verb, object)`, but more
specifically `(role, action, resource)`. These are a set of rules which
state that a given role has permission to perform some action on some
resource, where both the action and resource are composed of `matchers`.
These allow for things like wildcards and also specifiers. For example,
the following policy would mean that the role `org-user` has data
reading permissions on the specific organisation with ID `42`.

    (org-user, data:read, org/42)

Wildcards make it possible to have a bit more flexibility. For example,
the following policy would mean that the role `org-admin` has `create`
permission on all users within the org `42`:

    (org-admin, user:create, org/42:user/*)

Wildcards can also be used in actions as well as resources. This policy
would mean that the `superuser` role has all `org` permissions within
the organisation `42`:

    (superuser, org:*, org/42)

Requests take a similar form, but for two differences: firstly, they
cannot contain wildcards, and secondly, they are for sets of roles
rather than single roles (although the set could have length zero or
one). For example, the following request is for an actor with roles 27,
83, and 99 to delete the user with ID `19` from the organisation with ID
`42`:

    {
      roles: [ 27, 83, 99 ],
      action: "user:delete",
      resource: "org/42:user/19"
    }

The resolver isn't precious about how roles are identified, as long as
it can look them up in the policy database.

## Matching actions and resources

Matchers can be either specific or general. More general matchers will
match against more specific ones, but not vice versa (`matches` is a
non-commutative, binary operator).

Actions have two types of matcher, `ASpecific` for things like `read`
and `write`, and `AWildcard` for wildcards (`*`). Resources have
three types, which takes into account the fact that a resource can be
specified by an identifier, or generalised across all identifiers, these
are `RSpecific` (for a resource type and an identifier), `RAny` (for
a generalisation across all instances of a given resource type), and
`RWildcard` (for `*` wildcards). Actions can only be matched against
actions, and resources against resources.

## Project structure

Definitions for the different matchers can be found in `Matcher`. This
includes the `Hierarchy a` structure which maps a linear hierarchy of
matchers which may or may not have children.

In order to convert the specification language to matcher hierarchies,
there are textual parsers in `Parser`. Parsers convert things like
`org/42:user/*:*` into:

    Node { matcher=(RSpecific "org" "42")
         , child=(Node { matcher=(RAny "user")
                       , child=(Node { matcher=RWildcard,
                                     , child=EndNode })})}

In order to have persistent policies, some sort of storage is required.
Currently, PostgreSQL is the only supported policy storage system.
Database definitions for `Policy` can be found in `Source.Policy`.
