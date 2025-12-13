// Monaco Loader Initialization
// Load Monaco's AMD loader FIRST to avoid conflicts with jQuery and other AMD modules
// This file must be loaded before any other scripts that use AMD define()

(function() {
    // Prevent jQuery from registering as an AMD module
    // This must happen before jQuery loads
    if (typeof window !== 'undefined') {
        window.define_amd_jquery_disabled = true;
    }

    // Load Monaco loader script
    const loaderScript = document.createElement('script');
    loaderScript.src = './node_modules/monaco-editor/min/vs/loader.js';
    loaderScript.async = false; // Load synchronously to ensure it's first
    loaderScript.onerror = function() {
        console.error('Failed to load Monaco loader.js');
    };
    document.head.appendChild(loaderScript);
})();
