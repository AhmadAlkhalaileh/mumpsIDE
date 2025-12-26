function detectYottaDBPath(sshConn) {
  // Try to detect YottaDB installation by checking common paths
  const commonPaths = [
    '/usr/local/lib/yottadb/r138',
    '/usr/local/lib/yottadb/r136',
    '/usr/local/lib/yottadb/r134',
    '/usr/local/lib/yottadb/r132',
    '/opt/yottadb/current',
    '/opt/fis-gtm/YDB136',
    '/opt/fis-gtm/YDB138',
    '/usr/lib/fis-gtm/V6.3-011_x86_64',
    '/usr/lib/x86_64-linux-gnu/fis-gtm/V6.3-011_x86_64'
  ];

  // Try comprehensive search
  return new Promise((resolve) => {
    console.log('[SSH] Detecting YottaDB path...');

    // First try: which mumps or ydb
    sshConn.exec('which mumps 2>/dev/null || which ydb 2>/dev/null', (err, stream) => {
      if (err) {
        console.log('[SSH] which command failed, trying common paths');
        checkCommonPaths(0);
        return;
      }

      let output = '';
      stream.on('close', (code) => {
        const mumpsPath = output.trim();
        console.log(`[SSH] which result: "${mumpsPath}"`);
        if (mumpsPath && mumpsPath.includes('/') && !mumpsPath.includes('not found')) {
          // Extract directory from the full path
          const ydbPath = mumpsPath.substring(0, mumpsPath.lastIndexOf('/'));
          console.log(`[SSH] Found via which: ${ydbPath}`);
          resolve(ydbPath);
        } else {
          console.log('[SSH] which returned no valid path, trying common paths');
          checkCommonPaths(0);
        }
      }).on('data', (data) => {
        output += data.toString();
      });
    });

    function checkCommonPaths(index) {
      if (index >= commonPaths.length) {
        // Last resort: try to find mumps anywhere
        console.log('[SSH] Common paths exhausted, searching filesystem...');
        sshConn.exec('find /usr /opt -name mumps -type f 2>/dev/null | head -1', (err, stream) => {
          if (err) {
            console.log('[SSH] find command failed');
            resolve(null);
            return;
          }

          let output = '';
          stream.on('close', () => {
            const mumpsPath = output.trim();
            if (mumpsPath && mumpsPath.includes('/')) {
              const ydbPath = mumpsPath.substring(0, mumpsPath.lastIndexOf('/'));
              console.log(`[SSH] Found via find: ${ydbPath}`);
              resolve(ydbPath);
            } else {
              console.log('[SSH] No mumps executable found anywhere');
              resolve(null);
            }
          }).on('data', (data) => {
            output += data.toString();
          });
        });
        return;
      }

      const testPath = commonPaths[index];
      sshConn.exec(`test -f "${testPath}/mumps" && echo "found"`, (err, stream) => {
        if (err) {
          checkCommonPaths(index + 1);
          return;
        }

        let output = '';
        stream.on('close', () => {
          if (output.trim() === 'found') {
            console.log(`[SSH] Found at common path: ${testPath}`);
            resolve(testPath);
          } else {
            checkCommonPaths(index + 1);
          }
        }).on('data', (data) => {
          output += data.toString();
        });
      });
    }
  });
}

module.exports = {
  detectYottaDBPath
};
