import { promises as fs } from 'fs';
import path from 'path';
import { cwd } from 'process';

import * as C from 'fp-ts/lib/Console';
import * as E from 'fp-ts/lib/Either';
import * as TE from 'fp-ts/lib/TaskEither';
import { pipe } from 'fp-ts/lib/function';
import * as t from 'io-ts';
import { PathReporter } from 'io-ts/lib/PathReporter';

const ConfigCodec = t.type({
  generatorVersion: t.string,
  inputFiles: t.array(t.string),
  outputDirectory: t.string,
});

export type Config = t.TypeOf<typeof ConfigCodec>;

function decodeConfig(maybeConfig: unknown): E.Either<Error, Config> {
  const decodedConfig = ConfigCodec.decode(maybeConfig);
  return pipe(
    decodedConfig,
    E.mapLeft(
      () => new Error(JSON.stringify(PathReporter.report(decodedConfig)))
    )
  );
}

const getFileContents = (path: string) =>
  TE.tryCatch(() => fs.readFile(path, 'utf-8'), E.toError);

export function readConfig(configPath?: string): TE.TaskEither<Error, Config> {
  const p = configPath ?? path.join(cwd(), '.openapi-generator.json');
  return pipe(
    TE.of(p),
    TE.chainFirstIOK((path: string) => C.log(`Reading config from ${path}`)),
    TE.chain((path: string) => getFileContents(path)),
    TE.chainEitherK((jsonStr: string) =>
      E.tryCatch(() => JSON.parse(jsonStr), E.toError)
    ),
    TE.chainEitherK((maybeConfig: unknown) => decodeConfig(maybeConfig))
  );
}
