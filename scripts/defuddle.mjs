#!/usr/bin/env node

import { readFile, writeFile } from 'fs/promises';
import { Defuddle } from 'defuddle/node';
import { JSDOM } from 'jsdom';

const baseURL = 'https://example.com/';

const inputType = process.argv[2];
const inputPath = process.argv[3];
const outputFormat = process.argv[4];
const outputPath = process.argv[5];

const errorFmt = () => console.error('Usage: defuddle.mjs [url|html] [website.org|path/to/file.html] [markdown|html] (path/to/output.[md|html])');

if (inputType !== "url" && inputType !== "html") {
  errorFmt();
  process.exit(1);
}  

if (!inputPath || !(outputFormat === "markdown" || outputFormat === "html")) {
  errorFmt();
  process.exit(1);
}

try {
  const input = inputType === "html"
      ? await readFile(inputPath, 'utf8')
      : await JSDOM.fromURL(inputPath);
  const result = await Defuddle(
    input,
    // inputType === "html" ? baseURL : undefined,
    inputType === "html" ? baseURL : inputPath,
    { markdown: outputFormat == 'markdown', debug: false});

  if (result?.content) {
    if (outputPath) {
      await writeFile(outputPath, result.content, 'utf8');
    } else {
      process.stdout.write(result.content);
    }
  } else {
    console.error('Could not extract readable content.');
    process.exit(1);
  }
} catch (err) {
  console.error(`Error: ${err.message}`);
  process.exit(1);
}
