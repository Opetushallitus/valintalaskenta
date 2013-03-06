// Testacular configuration
// Generated on Mon Feb 25 2013 08:05:53 GMT+0200 (FLE Standard Time)

// base path, that will be used to resolve files and exclude
basePath = '../../main/webapp/';


// list of files / patterns to load in the browser
files = [
  ANGULAR_SCENARIO,
  ANGULAR_SCENARIO_ADAPTER,
  '../../test/ui/e2e/**/*.js',
];

// test results reporter to use
// possible values: 'dots', 'progress', 'junit'
reporters = ['progress'];

// enable / disable colors in the output (reporters and logs)
colors = true;

// level of logging
// possible values: LOG_DISABLE || LOG_ERROR || LOG_WARN || LOG_INFO || LOG_DEBUG
logLevel = LOG_DEBUG;


// enable / disable watching file and executing tests whenever any file changes
autoWatch = false;


// Start these browsers, currently available:
// - Chrome
// - ChromeCanary
// - Firefox
// - Opera
// - Safari (only Mac)
// - PhantomJS
// - IE (only Windows)
browsers = ['Chrome'];

// Continuous Integration mode
// if true, it capture browsers, run tests and exit
singleRun = true;

proxies = {
    '/': 'http://localhost:7878/valintalaskenta-ui/'
};
