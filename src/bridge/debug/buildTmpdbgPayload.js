module.exports = {
  buildTmpdbgPayload(safeUserCode, guardedUserCode, dbgLog) {
    // Parse the first user tag to check if it has parameters
    const firstTagMatch = safeUserCode.match(/^([A-Za-z%][A-Za-z0-9]*)(\([^)]*\))?/);
    const firstTag = firstTagMatch ? firstTagMatch[1] : '';
    const hasParams = firstTagMatch && firstTagMatch[2];

    // Add () to first tag definition if missing (required for extrinsic calls)
    let finalUserCode = guardedUserCode;
    if (firstTag && !hasParams) {
      // Add () to the label definition: "AAAA ; comment" becomes "AAAA() ; comment"
      const tagRegex = new RegExp(`^(${firstTag})([\\s;])`, 'm');
      finalUserCode = guardedUserCode.replace(tagRegex, `$1()$2`);
      dbgLog('[DEBUG] Added () to label definition for extrinsic call compatibility');
    }

    // Build the full code payload with TMPDBG tag, QUIT, and optionally a START tag
    let codePayload;
    let headerLines;
    let hasStartWrapper = false;

    if (firstTag) {
      hasStartWrapper = true;

      if (hasParams) {
        // If first tag has parameters, create entry that calls it with dummy values
        // Extract parameter count
        const paramList = firstTagMatch[2].slice(1, -1).split(',').map(p => p.trim()).filter(Boolean);
        const dummyArgs = paramList.map(() => '0').join(',');

        codePayload = [
          'TMPDBG ; Debug temp routine',
          ` NEW RET SET RET=$$${firstTag}(${dummyArgs})`,
          ' QUIT:$QUIT RET  QUIT',
          finalUserCode
        ].join('\n');
        headerLines = 3; // TMPDBG, SET RET=$$..., QUIT
      } else {
        // No parameters - use extrinsic call with () (we added () to definition above)
        codePayload = [
          'TMPDBG ; Debug temp routine',
          ` NEW RET SET RET=$$${firstTag}()`,
          ' QUIT:$QUIT RET  QUIT',
          finalUserCode
        ].join('\n');
        headerLines = 3; // TMPDBG, SET RET=$$..., QUIT
      }
    } else {
      // No tags found, use simple structure
      codePayload = `TMPDBG ; Debug temp routine\n QUIT:$QUIT 0  QUIT\n${finalUserCode}`;
      headerLines = 2; // TMPDBG, QUIT
    }

    return { firstTag, hasParams, codePayload, headerLines, hasStartWrapper };
  }
};

