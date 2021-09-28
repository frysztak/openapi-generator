import { sequenceS } from 'fp-ts/lib/Apply';
import { pipe } from 'fp-ts/lib/function';
import * as TE from 'fp-ts/lib/TaskEither';
import { ArgumentParser } from 'argparse';

import { readConfig } from './config';
import { prepareGenerator } from './generator';
import { runGenerator } from './runner';
const { version } = require('./../package.json');

const parser = new ArgumentParser({
  description: 'OpenAPI client generator',
  add_help: true,
});
parser.add_argument('-v', '--version', {
  action: 'version',
  version: `v${version}`,
});
const subparsers = parser.add_subparsers();
const generateParser = subparsers.add_parser('generate', {
  aliases: ['gen'],
  help: 'Generate clients',
});
generateParser.add_argument('-c', '--config', {
  required: false,
  help: 'Path to config JSON',
  default: 'auto',
});

interface CLIArgs {
  config?: string;
}

async function main() {
  const { config } = parser.parse_args() as CLIArgs;
  if (!config) {
    parser.print_usage();
    return;
  }
  await pipe(
    readConfig(config === 'auto' ? undefined : config),
    TE.chain((config) => {
      return sequenceS(TE.taskEither)({
        config: TE.of(config),
        generatorPath: prepareGenerator(config.generatorVersion),
      });
    }),
    TE.chain(({ generatorPath, config }) =>
      runGenerator(generatorPath, config)
    ),
    TE.fold(
      (e: Error) => {
        console.error(e);
        return TE.of(false);
      },
      () => {
        console.log('All done!');
        return TE.of(true);
      }
    )
  )();
}

main();
