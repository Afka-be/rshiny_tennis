const { task, series, src, dest, watch } = require('gulp');
const sass = require('gulp-sass')(require('sass'));
const cssnano = require('gulp-cssnano');
const rename = require('gulp-rename');
const autoprefixer = require('gulp-autoprefixer');
const sourcemaps = require('gulp-sourcemaps');

// Folders structure => Src and Dist directories
const dir = {
        src         : 'www/style/',  //dir.src outputs this path
        dist        : 'www/style/'   //dir.dist outputs this path
      }

/*
  -- TOP LEVEL FUNCTIONS 
  gulp.task - Define tasks
  gulp.src - Point to files to use
  gulp.dest - Points to folder to output
  gulp.watch - Watch files and folders for changes
  gulp.series - As of gulp V4, we need this to execute several tasks simultaneously
  We add "async" before functions because after gulp v4 to prevent the error "The following tasks did not complete: Did you forget to signal async completion?" when running gulp
*/

// Logs Message - Test to make sure that Gulp is running fine
task('message', async function(){
  return console.log('Gulp is running...');
});

// Initialize Sourcemaps, Compile SASS files into CSS, Add Autoprefixes for browser compatibilities, Minify with cssnano, rename as "style.min.css", Put Sourcemaps, Moved to dist/css, Reload browser if BrowserSync initialized
task('styles', async function(){
  src(dir.src + '/**/*.scss')
    .pipe (sourcemaps.init())
    .pipe(sass().on('error', sass.logError))
    .pipe(autoprefixer({
      "overrideBrowserslist": [
        "> 1%",
        "last 2 versions",
        "ie >= 11"
      ]
    }))
    //.pipe(cssnano())
    //.pipe(rename({ suffix: '.min' }))
    //.pipe(sourcemaps.write('./'))
    .pipe(dest(dir.dist))
});

// Default tasks. That is what Gulp will do if you just write "gulp" without any task name in the command console.
task('default', series(['styles']));

// Watch => Process all the following tasks in series every time those files are changed.
task('watch', async function() {
  watch(dir.src + '**/*.scss', series('styles'));
});