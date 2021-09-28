# Node.js OpenAPI Generator wrapper

Node.js wrapper for [OpenAPI Generator](https://github.com/frysztak/openapi-generator).

## Install

```sh
$ yarn add -D @frysztak/openapi-generator
```

or

```sh
$ npm install --save-dev @frysztak/openapi-generator
```

## Usage

Create `.openapi-generator.json` in your project root. Sample file:

```json
{
  "generatorVersion": "0.1.0",
  "inputFiles": ["src/api/Admin.json", "src/api/User.json"],
  "outputDirectory": "src/api/clients"
}
```

Add the following to your `scripts` section in `package.json`:

```json
"scripts": {
  "gen-api": "run-openapi-generator generate"
}
```

## Supported subcommands

- `generate` - generates clients. Args:
  - `--config` (`-c`) -- path to `.openapi-generator.json` config file. Optional, defaults to `cwd`.
