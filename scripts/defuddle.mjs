#!/usr/bin/env node

import { readFile, writeFile } from 'fs/promises';
import { Defuddle } from 'defuddle/node';
import { JSDOM } from 'jsdom';

const baseURL = 'https://example.com/';

const inputPath = process.argv[2];
const outputFormat = process.argv[3];
const outputPath = process.argv[4];

const errorFmt = () => console.error('Usage: defuddle.mjs path/to/file.html md/html path/to/output.{md,html}');

if (!inputPath || !(outputFormat === "markdown" || outputFormat === "html")) {
  errorFmt();
  process.exit(1);
}

try {
  const html = await readFile(inputPath, 'utf8');
  const result = await Defuddle(
  html, baseURL,
  { markdown: outputFormat == 'markdown', debug: true});

  if (result?.content) {
    await writeFile(outputPath, result.content, 'utf8');
  } else {
    console.error('Could not extract readable content.');
    process.exit(1);
  }
} catch (err) {
  console.error(`Error: ${err.message}`);
  process.exit(1);
}
