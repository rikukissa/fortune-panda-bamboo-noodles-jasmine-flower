const fileReaderStream = require('filereader-stream');
const { split } = require('event-stream');
const through2 = require('through2');

const Elm = require('elm/Main');
require('static/styles/main.scss');

const app = Elm.Main.embed(document.getElementById('main'));

app.ports.fileSelected.subscribe(function (id) {
  const node = document.getElementById(id);
  const file = node.files[0];

  fileReaderStream(file).pipe(through2((data, enc, callback) => {
    const reader = new FileReader();
    reader.onload = (function(event) {
      callback(null, event.target.result);
    });
    reader.readAsText(file);
  }))
  .pipe(split())
  .on('data', (row) =>
    app.ports.fileContentRead.send(row)
  );
});
