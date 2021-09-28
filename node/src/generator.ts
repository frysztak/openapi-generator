import { promises as fs } from 'fs';
import path from 'path';
import { arch, platform } from 'process';
import { request } from '@octokit/request';

import * as C from 'fp-ts/lib/Console';
import * as E from 'fp-ts/lib/Either';
import * as TE from 'fp-ts/lib/TaskEither';
import { pipe } from 'fp-ts/lib/function';

const owner = 'frysztak';
const repo = 'openapi-generator';

function getBinDir(): string {
  const packagePath = require.resolve(
    '@frysztak/openapi-generator/package.json'
  );
  const rootPath = path.dirname(packagePath);
  return path.join(rootPath, 'bin');
}

function getExePath(version: string): string {
  const binDir: string = getBinDir();
  const isWin: boolean = platform === 'win32';
  const ext = isWin ? '.exe' : '';
  return path.join(binDir, `openapi-generator-${version}${ext}`);
}

function isVersionInBinDir(version: string): TE.TaskEither<Error, void> {
  const exePath = getExePath(version);

  return pipe(TE.tryCatch(() => fs.access(exePath), E.toError));
}

function getArchSuffix(): string {
  return `${platform}-${arch}`;
}

function downloadGenerator(version: string): TE.TaskEither<Error, void> {
  return pipe(
    TE.tryCatch(
      () =>
        request('GET /repos/{owner}/{repo}/releases/tags/{tag}', {
          owner,
          repo,
          tag: `v${version}`,
        }),
      E.toError
    ),
    TE.chain((releaseResponse) => {
      const arch = getArchSuffix();
      const assetName = `openapi-generator-${arch}`;
      const asset = releaseResponse.data.assets.find(
        ({ name }) => name === assetName
      );
      return asset
        ? TE.right(asset)
        : TE.left(
            new Error(
              `No release found for version '${version}' and architecture '${arch}'`
            )
          );
    }),
    TE.chainFirstIOK(() => C.log('Downloading generator...')),
    TE.chain((asset) => {
      return TE.tryCatch(
        () =>
          request('GET /repos/{owner}/{repo}/releases/assets/{asset_id}', {
            owner,
            repo,
            asset_id: asset.id,
            headers: {
              Accept: 'application/octet-stream',
            },
          }),
        E.toError
      );
    }),
    TE.chainFirstIOK(() => C.log('Saving generator...')),
    TE.chain((assetStream) => {
      const arrayBuffer = assetStream.data as unknown as ArrayBuffer;
      const exePath = getExePath(version);
      return TE.tryCatch(
        () => fs.writeFile(exePath, Buffer.from(arrayBuffer)),
        E.toError
      );
    }),
    TE.chain(() =>
      TE.tryCatch(() => fs.chmod(getExePath(version), 0o755), E.toError)
    )
  );
}

export function prepareGenerator(
  version: string
): TE.TaskEither<Error, string> {
  return pipe(
    isVersionInBinDir(version),
    TE.chainFirstIOK(() =>
      C.log(`Generator found, version ${version}, arch ${getArchSuffix()}`)
    ),
    TE.orElse(() => downloadGenerator(version)),
    TE.map(() => getExePath(version))
  );
}
