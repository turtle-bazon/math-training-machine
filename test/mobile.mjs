// Throwaway mobile probe: phone viewport + touch emulation.
import { chromium } from 'playwright-core';
import { homedir } from 'node:os';
import { mkdirSync } from 'node:fs';

const BASE = process.argv[2] || 'http://127.0.0.1:8123';
const CHROME = `${homedir()}/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome`;
const SHOTS = '/tmp/opencode/shots';

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const parseQuestion = (t) => {
  const m = /\((-?\d+)\)\s*([+-])\s*\((-?\d+)\)\s*=\s*\?/.exec(t);
  if (!m) return null;
  const a = parseInt(m[1], 10), b = parseInt(m[3], 10);
  return m[2] === '+' ? a + b : a - b;
};

mkdirSync(SHOTS, { recursive: true });
const browser = await chromium.launch({
  executablePath: CHROME,
  headless: true,
  args: ['--no-sandbox', '--autoplay-policy=no-user-gesture-required'],
});
const ctx = await browser.newContext({
  viewport: { width: 390, height: 844 },   // iPhone 14-ish
  deviceScaleFactor: 3,
  isMobile: true,
  hasTouch: true,
  userAgent:
    'Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Mobile/15E148 Safari/604.1',
});
const page = await ctx.newPage();
const errors = [];
page.on('pageerror', (e) => errors.push(`pageerror: ${e.message}`));
page.on('console', (m) => { if (m.type() === 'error') errors.push(m.text()); });

const overflow = () =>
  page.evaluate(() => ({
    scrollW: document.documentElement.scrollWidth,
    innerW: window.innerWidth,
    bodyH: document.documentElement.scrollHeight,
    innerH: window.innerHeight,
  }));

await page.goto(`${BASE}/?rt=60`, { waitUntil: 'networkidle' });
console.log('menu overflow:', JSON.stringify(await overflow()));
await page.screenshot({ path: `${SHOTS}/m1-menu.png` });

// tap-target sizes of the small controls on the menu
const menuSizes = await page.evaluate(() => {
  const out = {};
  for (const s of ['.mode', '.icon-btn', '.primary.big', '.ghost']) {
    const el = document.querySelector(s);
    if (el) {
      const r = el.getBoundingClientRect();
      out[s] = `${Math.round(r.width)}x${Math.round(r.height)}`;
    }
  }
  return out;
});

// start a round with a TAP (not a click)
await page.tap('button.primary.big');
await page.waitForSelector('.question', { timeout: 5000 });
console.log('round started by tap: ok');
await sleep(400);
console.log('play overflow:', JSON.stringify(await overflow()));

// answer the current question with a touch tap on the correct option
const answer = parseQuestion(await page.textContent('.question'));
let tapped = false;
if (answer !== null) {
  const found = await page.$$eval('.option', (opts, val) => {
    const i = opts.findIndex(
      (o) => o.querySelector('.opt-value').textContent.trim() === String(val),
    );
    if (i >= 0) opts[i].setAttribute('data-target', '1');
    return i >= 0;
  }, answer);
  if (found) {
    await page.tap('.option[data-target="1"]');
    tapped = true;
    await sleep(200);   // feedback marker lives for 550ms
    console.log(
      'answer by tap accepted:',
      (await page.$$('.option.correct')).length === 1,
    );
  }
}
console.log('tapped option:', tapped);
console.log('menu tap targets:', JSON.stringify(menuSizes));
await page.screenshot({ path: `${SHOTS}/m2-play.png` });

// tap-target sizes of the small controls
const sizes = await page.evaluate(() => {
  const sel = ['.option', '.icon-btn', '.tab', '.primary.big', '.name-input'];
  const out = {};
  for (const s of sel) {
    const el = document.querySelector(s);
    if (el) {
      const r = el.getBoundingClientRect();
      out[s] = `${Math.round(r.width)}x${Math.round(r.height)}`;
    }
  }
  return out;
});
console.log('tap target sizes:', JSON.stringify(sizes));
console.log('js errors:', errors.length ? errors.slice(0, 4) : 'none');

await browser.close();
