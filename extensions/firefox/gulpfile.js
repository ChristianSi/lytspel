const gulp = require('gulp');
const source = require("vinyl-source-stream");
const rollup = require('rollup-stream');
const resolve = require('rollup-plugin-node-resolve');
const commonjs = require('rollup-plugin-commonjs');
const del = require('del');

const dist = './dist';

const scripts = [
    {taskName: 'buildBackground', entry: './src/background.js', source: 'background.js', dest: dist},
    {taskName: 'buildAction', entry: './src/action/action.js', source: 'action.js', dest: `${dist}/action`},
    {taskName: 'buildOptions', entry: './src/options/options.js', source: 'options.js', dest: `${dist}/options`}
];

gulp.task('default', ['build']);

gulp.task('clean', () =>
    del([`${dist}/**/*`, `${dist}/.*`, './web-ext-artifacts/']));

gulp.task('copyStaticContent', () =>
    gulp.src('./static/**')
    .pipe(gulp.dest("./dist")));

let rollupCache;

scripts.forEach(script =>
    gulp.task(script.taskName, () =>
        rollup({
            entry: script.entry,
            format: 'es',
            exports: 'none',
            plugins: [ resolve(), commonjs() ],
            cache: rollupCache
        })
        .on('unifiedcache', unifiedCache => rollupCache = unifiedCache)
        .pipe(source(script.source))
        .pipe(gulp.dest(script.dest))));

gulp.task('build', ['copyStaticContent'].concat(scripts.map(script => script.taskName)));
