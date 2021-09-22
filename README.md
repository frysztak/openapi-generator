# OpenAPI Generator
Simple OpenAPI client generator written in Haskell.

## Supported features
- JSON OpenAPI schema (no YAML)
- TypeScript generators: Interface, Fetch

## Sample usage
```sh
$ openapi-generator -i SchemaA.json -i SchemaB.json -o api
```

## Sample output
Running command above generates the following folder structure:
```
api
├── SchemaA
│   ├── fetch.ts
│   ├── index.ts
│   └── models.ts
├── SchemaB
│   ├── fetch.ts
│   ├── index.ts
│   └── models.ts
└── common.ts
```

`models.ts` includes all request/response interfaces, as well as enums (implemented as union of string literals), e.g.:
```ts
export interface Pet {
  name: string,
  photoUrls: string[],
  category?: Category,
  id?: number,
  tags?: Tag[]
};
```

`fetch.ts` includes all query functions, including cancellation (in a form suitable for [React-Query](https://react-query.tanstack.com/)):
```ts
import * as M from "./models";
import { buildUrl, buildHeaders } from "../common";

export const addPet = (config: FetchConfig) => (requestBody: M.Pet): Promise<M.Pet> => {
  const controller = new AbortController();
  const { signal } = controller;
  const { baseUrl } = config;
  const url = buildUrl(baseUrl, "/pet");
  const promise = new Promise<M.Pet>(async (resolve, reject) => {
    try {
      const response = await fetch(url, {
        signal,
        "method": "POST",
        "headers": buildHeaders(),
        "body": JSON.stringify(requestBody)
      });
      const json = await response.json();
      if (!response.ok) {
        reject({
          response,
          "body": json
        });
      }
      resolve(json);
    } catch (err) {
      reject(err);
    }
  });
  (promise as any).cancel = () => controller.abort();
  return promise;
};
```