const { getRoutineDirs } = require('../routines/fetchRoutineDirectoriesToLocal');
const { runHostCommand } = require('../util/runHostCommand');
const { buildSourceMapFromCode } = require('./sourceMapUtils');

const sourceMapCache = {};

async function loadRoutineSourceMap(routine, inlineSourceMap = null) {
  if (!routine) return null;
  const key = routine.toUpperCase();
  if (sourceMapCache[key]) return sourceMapCache[key];
  if (inlineSourceMap) {
    sourceMapCache[key] = inlineSourceMap;
    return inlineSourceMap;
  }
  const routineDirs = getRoutineDirs();
  for (const dir of routineDirs) {
    const cmd = `cat ${dir}/${key}.m 2>/dev/null`;
    const res = await runHostCommand(cmd);
    if (res.ok && res.stdout) {
      const smap = buildSourceMapFromCode(key, res.stdout);
      sourceMapCache[key] = smap;
      return smap;
    }
  }
  return null;
}

module.exports = {
  sourceMapCache,
  loadRoutineSourceMap
};

