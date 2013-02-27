// Testacular configuration
// Generated on Mon Feb 25 2013 08:05:53 GMT+0200 (FLE Standard Time)


// base path, that will be used to resolve files and exclude
basePath = '../../main/webapp/';


// list of files / patterns to load in the browser
files = [
  'jslib/angular-1.0.3/angular.js',
  'jslib/angular-1.0.3/angular-*.js',
  ANGULAR_SCENARIO,
  ANGULAR_SCENARIO_ADAPTER,
  JASMINE,
  JASMINE_ADAPTER,
  
  
  'js/valintalaskenta.js',
  'js/*.js',
  '../../test/ui/e2e/**/*.js',
  '../../test/ui/unit/**/*.js'
  
];


// list of files to exclude
exclude = [
  'js/virkailija.js'
];


// test results reporter to use
// possible values: 'dots', 'progress', 'junit'
reporters = ['progress'];


// web server port
port = 8088;


// cli runner port
runnerPort = 9100;


// enable / disable colors in the output (reporters and logs)
colors = true;


// level of logging
// possible values: LOG_DISABLE || LOG_ERROR || LOG_WARN || LOG_INFO || LOG_DEBUG
logLevel = LOG_INFO;


// enable / disable watching file and executing tests whenever any file changes
autoWatch = true;


// Start these browsers, currently available:
// - Chrome
// - ChromeCanary
// - Firefox
// - Opera
// - Safari (only Mac)
// - PhantomJS
// - IE (only Windows)
browsers = ['Chrome'];


// If browser does not capture in given timeout [ms], kill it
captureTimeout = 5000;


// Continuous Integration mode
// if true, it capture browsers, run tests and exit
singleRun = false;