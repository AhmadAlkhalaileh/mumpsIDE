(() => {
    const TABLER_VERSION = '2.47.0';

    const TABLER_SVGS = {
        'alert-triangle': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M12 9v4" /> <path d="M10.363 3.591l-8.106 13.534a1.914 1.914 0 0 0 1.636 2.871h16.214a1.914 1.914 0 0 0 1.636 -2.87l-8.106 -13.536a1.914 1.914 0 0 0 -3.274 0z" /> <path d="M12 16h.01" /> </svg>',
        'align-justified': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M4 6l16 0" /> <path d="M4 12l16 0" /> <path d="M4 18l12 0" /> </svg>',
        'archive': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M3 4m0 2a2 2 0 0 1 2 -2h14a2 2 0 0 1 2 2v0a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2z" /> <path d="M5 8v10a2 2 0 0 0 2 2h10a2 2 0 0 0 2 -2v-10" /> <path d="M10 12l4 0" /> </svg>',
        'archive-off': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M8 4h11a2 2 0 1 1 0 4h-7m-4 0h-3a2 2 0 0 1 -.826 -3.822" /> <path d="M5 8v10a2 2 0 0 0 2 2h10a2 2 0 0 0 1.824 -1.18m.176 -3.82v-7" /> <path d="M10 12h2" /> <path d="M3 3l18 18" /> </svg>',
        'arrow-back-up': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M9 14l-4 -4l4 -4" /> <path d="M5 10h11a4 4 0 1 1 0 8h-1" /> </svg>',
        'arrow-right': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M5 12l14 0" /> <path d="M13 18l6 -6" /> <path d="M13 6l6 6" /> </svg>',
        'bug': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M9 9v-1a3 3 0 0 1 6 0v1" /> <path d="M8 9h8a6 6 0 0 1 1 3v3a5 5 0 0 1 -10 0v-3a6 6 0 0 1 1 -3" /> <path d="M3 13l4 0" /> <path d="M17 13l4 0" /> <path d="M12 20l0 -6" /> <path d="M4 19l3.35 -2" /> <path d="M20 19l-3.35 -2" /> <path d="M4 7l3.75 2.4" /> <path d="M20 7l-3.75 2.4" /> </svg>',
        'bulb': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M3 12h1m8 -9v1m8 8h1m-15.4 -6.4l.7 .7m12.1 -.7l-.7 .7" /> <path d="M9 16a5 5 0 1 1 6 0a3.5 3.5 0 0 0 -1 3a2 2 0 0 1 -4 0a3.5 3.5 0 0 0 -1 -3" /> <path d="M9.7 17l4.6 0" /> </svg>',
        'calendar': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M4 7a2 2 0 0 1 2 -2h12a2 2 0 0 1 2 2v12a2 2 0 0 1 -2 2h-12a2 2 0 0 1 -2 -2v-12z" /> <path d="M16 3v4" /> <path d="M8 3v4" /> <path d="M4 11h16" /> <path d="M11 15h1" /> <path d="M12 15v3" /> </svg>',
        'chart-histogram': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M3 3v18h18" /> <path d="M20 18v3" /> <path d="M16 16v5" /> <path d="M12 13v8" /> <path d="M8 16v5" /> <path d="M3 11c6 0 5 -5 9 -5s3 5 9 5" /> </svg>',
        'check': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M5 12l5 5l10 -10" /> </svg>',
        'chevron-down': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M6 9l6 6l6 -6" /> </svg>',
        'chevron-right': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M9 6l6 6l-6 6" /> </svg>',
        'chevron-up': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M6 15l6 -6l6 6" /> </svg>',
        'circle-dot': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M12 12m-1 0a1 1 0 1 0 2 0a1 1 0 1 0 -2 0" /> <path d="M12 12m-9 0a9 9 0 1 0 18 0a9 9 0 1 0 -18 0" /> </svg>',
        'circle-x': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M12 12m-9 0a9 9 0 1 0 18 0a9 9 0 1 0 -18 0" /> <path d="M10 10l4 4m0 -4l-4 4" /> </svg>',
        'clipboard': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M9 5h-2a2 2 0 0 0 -2 2v12a2 2 0 0 0 2 2h10a2 2 0 0 0 2 -2v-12a2 2 0 0 0 -2 -2h-2" /> <path d="M9 3m0 2a2 2 0 0 1 2 -2h2a2 2 0 0 1 2 2v0a2 2 0 0 1 -2 2h-2a2 2 0 0 1 -2 -2z" /> </svg>',
        'clock': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M12 7v5l3 3" /> <path d="M3 12a9 9 0 1 0 18 0a9 9 0 0 0 -18 0" /> </svg>',
        'cloud': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M6.657 18c-2.572 0 -4.657 -2.007 -4.657 -4.483c0 -2.475 2.085 -4.482 4.657 -4.482c.393 -1.762 1.794 -3.2 3.675 -3.773c1.88 -.572 3.956 -.193 5.444 1c1.488 1.19 2.162 3.007 1.77 4.769h.99c1.913 0 3.464 1.56 3.464 3.486c0 1.927 -1.551 3.487 -3.465 3.487h-11.878" /> </svg>',
        'copy': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M7 7m0 2.667a2.667 2.667 0 0 1 2.667 -2.667h8.666a2.667 2.667 0 0 1 2.667 2.667v8.666a2.667 2.667 0 0 1 -2.667 2.667h-8.666a2.667 2.667 0 0 1 -2.667 -2.667z" /> <path d="M4.012 16.737a2.005 2.005 0 0 1 -1.012 -1.737v-10c0 -1.1 .9 -2 2 -2h10c.75 0 1.158 .385 1.5 1" /> </svg>',
        'device-floppy': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M6 4h10l4 4v10a2 2 0 0 1 -2 2h-12a2 2 0 0 1 -2 -2v-12a2 2 0 0 1 2 -2" /> <path d="M12 14m-2 0a2 2 0 1 0 4 0a2 2 0 1 0 -4 0" /> <path d="M14 4l0 4l-6 0l0 -4" /> </svg>',
        'download': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M4 17v2a2 2 0 0 0 2 2h12a2 2 0 0 0 2 -2v-2" /> <path d="M7 11l5 5l5 -5" /> <path d="M12 4l0 12" /> </svg>',
        'dots-vertical': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M12 5m-1 0a1 1 0 1 0 2 0a1 1 0 1 0 -2 0" /> <path d="M12 12m-1 0a1 1 0 1 0 2 0a1 1 0 1 0 -2 0" /> <path d="M12 19m-1 0a1 1 0 1 0 2 0a1 1 0 1 0 -2 0" /> </svg>',
        'edit': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M7 7h-1a2 2 0 0 0 -2 2v9a2 2 0 0 0 2 2h9a2 2 0 0 0 2 -2v-1" /> <path d="M20.385 6.585a2.1 2.1 0 0 0 -2.97 -2.97l-8.415 8.385v3h3l8.385 -8.415z" /> <path d="M16 5l3 3" /> </svg>',
        'eye': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M1 12s4 -8 11 -8s11 8 11 8s-4 8 -11 8s-11 -8 -11 -8" /> <circle cx="12" cy="12" r="3" /> </svg>',
        'file-diff': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M14 3v4a1 1 0 0 0 1 1h4" /> <path d="M17 21h-10a2 2 0 0 1 -2 -2v-14a2 2 0 0 1 2 -2h7l5 5v11a2 2 0 0 1 -2 2z" /> <path d="M12 10l0 4" /> <path d="M10 12l4 0" /> <path d="M10 17l4 0" /> </svg>',
        'filter': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M4 4h16v2.172a2 2 0 0 1 -.586 1.414l-4.414 4.414v7l-6 2v-8.5l-4.48 -4.928a2 2 0 0 1 -.52 -1.345v-2.227z" /> </svg>',
        'flame': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M12 3c2 2 4 5 4 8a4 4 0 1 1 -8 0c0 -3 2 -6 4 -8z" /> </svg>',
        'folder': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M5 4h4l3 3h7a2 2 0 0 1 2 2v8a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2v-11a2 2 0 0 1 2 -2" /> </svg>',
        'folder-open': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M5 19l2.757 -7.351a1 1 0 0 1 .936 -.649h12.307a1 1 0 0 1 .986 1.164l-.996 5.211a2 2 0 0 1 -1.964 1.625h-14.026a2 2 0 0 1 -2 -2v-11a2 2 0 0 1 2 -2h4l3 3h7a2 2 0 0 1 2 2v2" /> </svg>',
        'git-branch': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M7 18m-2 0a2 2 0 1 0 4 0a2 2 0 1 0 -4 0" /> <path d="M7 6m-2 0a2 2 0 1 0 4 0a2 2 0 1 0 -4 0" /> <path d="M17 6m-2 0a2 2 0 1 0 4 0a2 2 0 1 0 -4 0" /> <path d="M7 8l0 8" /> <path d="M9 18h6a2 2 0 0 0 2 -2v-5" /> <path d="M14 14l3 -3l3 3" /> </svg>',
        'git-commit': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M12 12m-3 0a3 3 0 1 0 6 0a3 3 0 1 0 -6 0" /> <path d="M12 3l0 6" /> <path d="M12 15l0 6" /> </svg>',
        'history': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M12 8l0 4l2 2" /> <path d="M3.05 11a9 9 0 1 1 .5 4m-.5 5v-5h5" /> </svg>',
        'info-circle': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M3 12a9 9 0 1 0 18 0a9 9 0 0 0 -18 0" /> <path d="M12 9h.01" /> <path d="M11 12h1v4h1" /> </svg>',
        'link': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M9 15l6 -6" /> <path d="M11 6l.463 -.536a5 5 0 0 1 7.071 7.072l-.534 .464" /> <path d="M13 18l-.397 .534a5.068 5.068 0 0 1 -7.127 0a4.972 4.972 0 0 1 0 -7.071l.524 -.463" /> </svg>',
        'logout': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M14 8v-2a2 2 0 0 0 -2 -2h-7a2 2 0 0 0 -2 2v12a2 2 0 0 0 2 2h7a2 2 0 0 0 2 -2v-2" /> <path d="M9 12h12l-3 -3" /> <path d="M18 15l3 -3" /> </svg>',
        'message': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M8 9h8" /> <path d="M8 13h6" /> <path d="M18 4a3 3 0 0 1 3 3v8a3 3 0 0 1 -3 3h-5l-5 3v-3h-2a3 3 0 0 1 -3 -3v-8a3 3 0 0 1 3 -3h12z" /> </svg>',
        'minus': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M5 12l14 0" /> </svg>',
        'player-play': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M7 4v16l13 -8z" /> </svg>',
        'plus': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M12 5l0 14" /> <path d="M5 12l14 0" /> </svg>',
        'refresh': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M20 11a8.1 8.1 0 0 0 -15.5 -2m-.5 -4v4h4" /> <path d="M4 13a8.1 8.1 0 0 0 15.5 2m.5 4v-4h-4" /> </svg>',
        'scissors': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M6 7m-3 0a3 3 0 1 0 6 0a3 3 0 1 0 -6 0" /> <path d="M6 17m-3 0a3 3 0 1 0 6 0a3 3 0 1 0 -6 0" /> <path d="M8.6 8.6l10.4 10.4" /> <path d="M8.6 15.4l10.4 -10.4" /> </svg>',
        'search': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M10 10m-7 0a7 7 0 1 0 14 0a7 7 0 1 0 -14 0" /> <path d="M21 21l-6 -6" /> </svg>',
        'settings': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M10.325 4.317c.426 -1.756 2.924 -1.756 3.35 0a1.724 1.724 0 0 0 2.573 1.066c1.543 -.94 3.31 .826 2.37 2.37a1.724 1.724 0 0 0 1.065 2.572c1.756 .426 1.756 2.924 0 3.35a1.724 1.724 0 0 0 -1.066 2.573c.94 1.543 -.826 3.31 -2.37 2.37a1.724 1.724 0 0 0 -2.572 1.065c-.426 1.756 -2.924 1.756 -3.35 0a1.724 1.724 0 0 0 -2.573 -1.066c-1.543 .94 -3.31 -.826 -2.37 -2.37a1.724 1.724 0 0 0 -1.065 -2.572c-1.756 -.426 -1.756 -2.924 0 -3.35a1.724 1.724 0 0 0 1.066 -2.573c-.94 -1.543 .826 -3.31 2.37 -2.37c1 .608 2.296 .07 2.572 -1.065z" /> <path d="M9 12a3 3 0 1 0 6 0a3 3 0 0 0 -6 0" /> </svg>',
        'square': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M3 3m0 2a2 2 0 0 1 2 -2h14a2 2 0 0 1 2 2v14a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2z" /> </svg>',
        'square-arrow-right': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M5 3h14a2 2 0 0 1 2 2v14a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2v-14a2 2 0 0 1 2 -2" /> <path d="M10 12h7" /> <path d="M14 9l3 3l-3 3" /> </svg>',
        'square-check': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M3 3m0 2a2 2 0 0 1 2 -2h14a2 2 0 0 1 2 2v14a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2z" /> <path d="M9 12l2 2l4 -4" /> </svg>',
        'square-minus': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M12.5 21h-7.5a2 2 0 0 1 -2 -2v-14a2 2 0 0 1 2 -2h14a2 2 0 0 1 2 2v10" /> <path d="M16 19h6" /> </svg>',
        'terminal-2': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M8 9l3 3l-3 3" /> <path d="M13 15l3 0" /> <path d="M3 4m0 2a2 2 0 0 1 2 -2h14a2 2 0 0 1 2 2v12a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2z" /> </svg>',
        'upload': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M4 17v2a2 2 0 0 0 2 2h12a2 2 0 0 0 2 -2v-2" /> <path d="M7 9l5 -5l5 5" /> <path d="M12 4l0 12" /> </svg>',
        'user': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M8 7a4 4 0 1 0 8 0a4 4 0 0 0 -8 0" /> <path d="M6 21v-2a4 4 0 0 1 4 -4h4a4 4 0 0 1 4 4v2" /> </svg>',
        'x': '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"> <path stroke="none" d="M0 0h24v24H0z" fill="none"/> <path d="M18 6l-12 12" /> <path d="M6 6l12 12" /> </svg>',
    };

    const ICONS = Object.freeze({
        'add': TABLER_SVGS['plus'],
        'apply': TABLER_SVGS['check'],
        'arrow-right': TABLER_SVGS['arrow-right'],
        'branch': TABLER_SVGS['git-branch'],
        'bulb': TABLER_SVGS['bulb'],
        'calendar': TABLER_SVGS['calendar'],
        'check': TABLER_SVGS['check'],
        'chevron-down': TABLER_SVGS['chevron-down'],
        'checkout': TABLER_SVGS['arrow-right'],
        'chevron-right': TABLER_SVGS['chevron-right'],
        'chevron-up': TABLER_SVGS['chevron-up'],
        'close': TABLER_SVGS['x'],
        'clock': TABLER_SVGS['clock'],
        'comment': TABLER_SVGS['message'],
        'commit': TABLER_SVGS['git-commit'],
        'copy': TABLER_SVGS['copy'],
        'cut': TABLER_SVGS['scissors'],
        'debug': TABLER_SVGS['bug'],
        'diff': TABLER_SVGS['file-diff'],
        'download': TABLER_SVGS['download'],
        'dots-vertical': TABLER_SVGS['dots-vertical'],
        'edit': TABLER_SVGS['edit'],
        'error': TABLER_SVGS['circle-x'],
        'exit': TABLER_SVGS['logout'],
        'eye': TABLER_SVGS['eye'],
        'fetch': TABLER_SVGS['refresh'],
        'filter': TABLER_SVGS['filter'],
        'flame': TABLER_SVGS['flame'],
        'folder': TABLER_SVGS['folder'],
        'folder-open': TABLER_SVGS['folder-open'],
        'format': TABLER_SVGS['align-justified'],
        'git': TABLER_SVGS['git-branch'],
        'graph': TABLER_SVGS['chart-histogram'],
        'history': TABLER_SVGS['history'],
        'info': TABLER_SVGS['info-circle'],
        'link': TABLER_SVGS['link'],
        'minus': TABLER_SVGS['minus'],
        'paste': TABLER_SVGS['clipboard'],
        'pop': TABLER_SVGS['archive-off'],
        'pull': TABLER_SVGS['download'],
        'push': TABLER_SVGS['upload'],
        'radio': TABLER_SVGS['circle-dot'],
        'refresh': TABLER_SVGS['refresh'],
        'remote': TABLER_SVGS['cloud'],
        'rollback': TABLER_SVGS['arrow-back-up'],
        'run': TABLER_SVGS['player-play'],
        'save': TABLER_SVGS['device-floppy'],
        'search': TABLER_SVGS['search'],
        'settings': TABLER_SVGS['settings'],
        'stage': TABLER_SVGS['square-check'],
        'square-arrow-right': TABLER_SVGS['square-arrow-right'],
        'stash': TABLER_SVGS['archive'],
        'stop': TABLER_SVGS['square'],
        'terminal': TABLER_SVGS['terminal-2'],
        'undo': TABLER_SVGS['arrow-back-up'],
        'unstage': TABLER_SVGS['square-minus'],
        'upload': TABLER_SVGS['upload'],
        'user': TABLER_SVGS['user'],
        'warning': TABLER_SVGS['alert-triangle'],
    });

    const GIT_ACTION_MAP = Object.freeze({
        stage: 'stage',
        unstage: 'unstage',
        commit: 'commit',
        push: 'push',
        pull: 'pull',
        fetch: 'fetch',
        branch: 'branch',
        checkout: 'checkout',
        stash: 'stash',
        pop: 'pop',
        apply: 'apply',
        refresh: 'refresh',
        filter: 'filter',
        settings: 'settings'
    });

    const normalizeName = (name) => String(name || '').trim().toLowerCase();

    const parseSvg = (markup) => {
        const tpl = document.createElement('template');
        tpl.innerHTML = markup.trim();
        return tpl.content.firstElementChild;
    };

    const sizeToVar = (size) => {
        if (size === 12) return 'var(--ui-icon-12)';
        if (size === 16) return 'var(--ui-icon-16)';
        if (size === 20) return 'var(--ui-icon-20)';
        return `${size}px`;
    };

    const createSvgElement = (name, opts = {}) => {
        const iconName = normalizeName(name);
        const markup = ICONS[iconName];
        if (!markup) return document.createElement('span');

        const size = Number(opts.size || 16) || 16;
        const title = opts.title ? String(opts.title) : '';
        const cls = String(opts.className || '').trim();

        const svg = parseSvg(markup);
        if (!svg) return document.createElement('span');

        svg.setAttribute('width', String(size));
        svg.setAttribute('height', String(size));
        svg.style.width = sizeToVar(size);
        svg.style.height = sizeToVar(size);
        svg.setAttribute('aria-hidden', title ? 'false' : 'true');
        svg.setAttribute('focusable', 'false');
        svg.classList.add('ui-icon', `ui-icon--${size}`);
        if (cls) cls.split(/\s+/).forEach((c) => c && svg.classList.add(c));

        if (title) {
            const titleEl = document.createElementNS('http://www.w3.org/2000/svg', 'title');
            titleEl.textContent = title;
            svg.insertBefore(titleEl, svg.firstChild);
        }

        return svg;
    };

    const replaceGitEmptyIcons = () => {
        if (typeof document === 'undefined') return;
        document.querySelectorAll('.git-log-empty svg').forEach((svg) => {
            if (svg?.dataset?.uiIconSource === 'map') return;
            const icon = createSvgElement('bulb', { size: 48 });
            if (!icon) return;
            icon.dataset.uiIconSource = 'map';
            svg.replaceWith(icon);
        });
    };

    const observeGitEmptyIcons = () => {
        replaceGitEmptyIcons();
        if (typeof MutationObserver === 'undefined') return;
        const root = document.body || document.documentElement;
        if (!root) return;
        const observer = new MutationObserver(() => replaceGitEmptyIcons());
        observer.observe(root, { childList: true, subtree: true });
    };

    if (typeof document !== 'undefined') {
        if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', observeGitEmptyIcons, { once: true });
        } else {
            observeGitEmptyIcons();
        }
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.icons = window.AhmadIDEModules.ui.icons || {};
        window.AhmadIDEModules.ui.icons.map = ICONS;
        window.AhmadIDEModules.ui.icons.gitActionMap = GIT_ACTION_MAP;
        window.AhmadIDEModules.ui.icons.tablerVersion = TABLER_VERSION;
        window.AhmadIDEModules.ui.icons.createSvgElement = createSvgElement;
    }
})();
