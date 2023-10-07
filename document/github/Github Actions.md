### Github Package
- Tips
  -  Should delete the same version firstly to forbide conflict.
  -  [GraphQL API Explorer](https://docs.github.com/en/graphql/overview/explorer) 
  -  Examples to get packageInf through GraphQL API
```
query getPackages($owner: String!, $repo: String!, $first: Int!) {
  repository(owner: $owner, name: $repo) {
    packages(first: $first) {
      edges {
        node {
          name
          id
          latestVersion {
            id
            version
          }
          versions(first: $first) {
            edges {
              node {
                id
              }
            }
          }
        }
      }
      pageInfo {
        endCursor
        hasNextPage
      }
    }
  }
}
```
