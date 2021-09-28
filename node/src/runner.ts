import { promisify } from 'util';
import { exec as execCb } from 'child_process';
import * as TE from 'fp-ts/lib/TaskEither';
import * as E from 'fp-ts/lib/Either';
import * as C from 'fp-ts/lib/Console';
import { pipe } from 'fp-ts/lib/function';

import { Config } from './config';

const exec = promisify(execCb);

function prepareArgs(config: Config): string {
  const { inputFiles, outputDirectory } = config;

  const inputArgs = inputFiles.reduce((acc, inputFile) => {
    return acc + ` -i ${inputFile}`;
  }, '');
  const outputArgs = ` -o ${outputDirectory}`;

  return inputArgs + outputArgs;
}

export function runGenerator(
  generatorPath: string,
  config: Config
): TE.TaskEither<Error, void> {
  const args = prepareArgs(config);
  return pipe(
    TE.tryCatch(
      () => exec(`${generatorPath} ${args}`, { windowsHide: true }),
      E.toError
    ),
    TE.chainFirstIOK(({ stdout }) => C.log(stdout)),
    TE.chainFirstIOK(({ stderr }) => C.error(stderr)),
    TE.map(() => void 0)
  );
}
