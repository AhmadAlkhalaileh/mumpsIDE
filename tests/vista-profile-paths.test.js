const test = require('node:test');
const assert = require('node:assert/strict');

const { extractVistaPathsFromProfileText } = require('../src/bridge/vista/vistaProfilePaths');

test('vista-profile: extracts gldPath + localr from simple exports', () => {
  const text = [
    'export gtmgbldir="/var/worldvista/prod/globals/mumps.gld"',
    'export gtmroutines="/var/worldvista/prod/localr /opt/fis-gtm/YDB136"',
    ''
  ].join('\n');

  const res = extractVistaPathsFromProfileText(text);
  assert.equal(res.gldPath, '/var/worldvista/prod/globals/mumps.gld');
  assert.equal(res.localrPath, '/var/worldvista/prod/localr');
  assert.equal(res.routinesPath, null);
  assert.equal(res.basePath, '/var/worldvista/prod');
  assert.equal(res.envKey, null);
});

test('vista-profile: extracts localr+routines and envKey from paren entries', () => {
  const text = [
    'export ydb_gbldir="/var/worldvista/prod/h/globals/mumps.gld"',
    'export ydb_routines="/var/worldvista/prod/h/localr(/var/worldvista/prod/h/localr) /var/worldvista/prod/h/routines(/var/worldvista/prod/h/routines) /opt/fis-gtm/YDB136"',
    ''
  ].join('\n');

  const res = extractVistaPathsFromProfileText(text);
  assert.equal(res.gldPath, '/var/worldvista/prod/h/globals/mumps.gld');
  assert.equal(res.localrPath, '/var/worldvista/prod/h/localr');
  assert.equal(res.routinesPath, '/var/worldvista/prod/h/routines');
  assert.equal(res.basePath, '/var/worldvista/prod/h');
  assert.equal(res.envKey, 'h');
});

test('vista-profile: expands variables used in paths', () => {
  const text = [
    'export VISTA_BASE="/var/worldvista/prod/h"',
    'export ydb_gbldir="$VISTA_BASE/globals/mumps.gld"',
    'export ydb_routines="$VISTA_BASE/localr($VISTA_BASE/localr) $VISTA_BASE/routines($VISTA_BASE/routines) $ydb_dist/libgtmutil.so $ydb_dist"',
    'export ydb_dist="/opt/fis-gtm/YDB136"',
    ''
  ].join('\n');

  const res = extractVistaPathsFromProfileText(text);
  assert.equal(res.gldPath, '/var/worldvista/prod/h/globals/mumps.gld');
  assert.equal(res.localrPath, '/var/worldvista/prod/h/localr');
  assert.equal(res.routinesPath, '/var/worldvista/prod/h/routines');
  assert.equal(res.basePath, '/var/worldvista/prod/h');
  assert.equal(res.envKey, 'h');
});
