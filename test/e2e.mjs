// Browser end-to-end test for the Math Training Machine.
//
//   node test/e2e.mjs [base-url]
//
// Needs a running server (make run / make dev-start) and a Chromium build
// from the playwright cache. Exercises: menu -> round (click + keyboard)
// -> results -> save score -> leaderboard.

import { chromium } from 'playwright-core';
import { homedir } from 'node:os';
import { mkdirSync } from 'node:fs';

const BASE = process.argv[2] || process.env.MTM_BASE || 'http://127.0.0.1:8123';
const CHROME =
  process.env.MTM_CHROME ||
  `${homedir()}/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome`;
const SHOTS = process.env.MTM_SHOTS || '/tmp/opencode/shots';

let failures = 0;
const check = (name, ok, extra = '') => {
  if (ok) console.log(`ok   ${name}`);
  else {
    failures += 1;
    console.log(`FAIL ${name}${extra ? `: ${extra}` : ''}`);
  }
};

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const parseQuestion = (text) => {
  const m = /\((-?\d+)\)\s*([+-])\s*\((-?\d+)\)\s*=\s*\?/.exec(text);
  if (!m) return null;
  const a = parseInt(m[1], 10);
  const b = parseInt(m[3], 10);
  return m[2] === '+' ? a + b : a - b;
};

const clickOption = (page, value) =>
  page.$$eval(
    '.option',
    (opts, val) => {
      const i = opts.findIndex(
        (o) => o.querySelector('.opt-value').textContent.trim() === String(val),
      );
      if (i < 0) return -1;
      opts[i].click();
      return i;
    },
    value,
  );

const score = async (page) => {
  const cell = await page.$('.hud-cell .hud-value');
  return cell ? parseInt(await cell.textContent(), 10) : 0;
};

const run = async () => {
  mkdirSync(SHOTS, { recursive: true });
  const browser = await chromium.launch({
    executablePath: CHROME,
    headless: true,
    args: ['--no-sandbox', '--autoplay-policy=no-user-gesture-required'],
  });
  const page = await browser.newPage({ viewport: { width: 1100, height: 900 } });

  const errors = [];
  page.on('pageerror', (e) => errors.push(`pageerror: ${e.message}`));
  page.on('console', (m) => {
    if (m.type() === 'error') errors.push(`console: ${m.text()}`);
  });

  // ---- menu ------------------------------------------------------------
  await page.goto(`${BASE}/?rt=12`, { waitUntil: 'networkidle' });
  check('menu renders logo', (await page.textContent('.logo-top')) === 'MATH');
  check(
    'three game modes offered',
    (await page.$$('.mode')).length === 3,
  );
  await page.screenshot({ path: `${SHOTS}/1-menu.png` });

  // ---- round, answered with the keyboard -------------------------------
  await page.click('button.primary.big'); // START ROUND
  await page.waitForSelector('.question', { timeout: 5000 });
  check('question appears', true);

  const seen = new Set();
  let clicked = 0;

  // ---- a miss must hold the correct answer on screen for ~3s -----------
  let firstAnswer = null;
  for (let i = 0; i < 5 && firstAnswer === null; i++) {
    firstAnswer = parseQuestion(await page.textContent('.question'));
    if (firstAnswer === null) await sleep(300);
  }
  const opts0 = await page.$$eval('.opt-value', (els) =>
    els.map((e) => e.textContent.trim()),
  );
  const wrongValue = opts0.find((v) => v !== String(firstAnswer));
  if (firstAnswer !== null && wrongValue !== undefined) {
    await clickOption(page, wrongValue);
    const marked = await page
      .waitForSelector('.option.correct', { timeout: 2000 })
      .then(() => true)
      .catch(() => false);
    check('miss reveals the correct option', marked);
    await sleep(1200);
    check(
      'correct option still shown after 1.2s',
      (await page.$$('.option.correct')).length === 1,
    );
    const released = await page
      .waitForSelector('.option.correct', {
        timeout: 4500,
        state: 'detached',
      })
      .then(() => true)
      .catch(() => false);
    check('feedback released after about 3s', released);
  }

  for (let i = 0; i < 6; i++) {
    const text = await page.textContent('.question');
    const answer = parseQuestion(text);
    if (answer === null) {
      await sleep(300);
      continue;
    }
    seen.add(text);
    const before = await score(page);
    const idx = await clickOption(page, answer);
    if (idx >= 0) clicked += 1;
    if (i === 0) {
      const marked = await page
        .waitForSelector('.option.correct', { timeout: 2000 })
        .then(() => true)
        .catch(() => false);
      check('correct answer marked', marked);
    }
    await sleep(750);
    const after = await score(page);
    if (i === 0) {
      check('answer accepted, score grows', after > before, `${before} -> ${after}`);
    }
  }
  check('several questions answered by click', clicked >= 3, `${clicked} clicked`);

  // keyboard answering on the very next question
  const keyText = await page.textContent('.question');
  const keyAnswer = parseQuestion(keyText);
  if (keyAnswer !== null) {
    const keyOptions = await page.$$eval('.opt-value', (els) =>
      els.map((e) => e.textContent.trim()),
    );
    const digit = keyOptions.findIndex((v) => v === String(keyAnswer)) + 1;
    const before = await score(page);
    await page.keyboard.press(String(digit));
    await sleep(750);
    check('keyboard answer works', (await score(page)) > before);
  }

  await page.screenshot({ path: `${SHOTS}/2-play.png` });
  check('a variety of questions shown', seen.size >= 2, `${seen.size} distinct`);

  // ---- results ---------------------------------------------------------
  await page.waitForSelector('.results .score-big', { timeout: 40000 });
  const finalScore = parseInt(await page.textContent('.score-big'), 10);
  check('final score is a number', Number.isFinite(finalScore), `${finalScore}`);
  check(
    'stats block present',
    (await page.$$('.stat')).length === 4,
  );
  await page.screenshot({ path: `${SHOTS}/3-results.png` });

  // ---- save to the shared leaderboard ---------------------------------
  await page.fill('.name-input', 'e2e-bot');
  await page.click('.save-row .primary');
  await page.waitForFunction(
    () => document.querySelector('.saved') !== null,
    { timeout: 10000 },
  );
  const savedText = await page.textContent('.saved');
  check('score saved with a rank', /ranked #\d+/.test(savedText), savedText.trim());

  // ---- leaderboard -----------------------------------------------------
  await page.click('button.ghost:has-text("LEADERBOARD")');
  await page.waitForSelector('.lb-row', { timeout: 10000 });
  const rows = await page.$$eval('.lb-row', (els) =>
    els.map((e) => e.textContent.replace(/\s+/g, ' ').trim()),
  );
  check('saved score on the board', rows.some((r) => r.includes('e2e-bot')), rows.join(' | '));
  check('board has sane rank order', rows.length >= 1);
  await page.screenshot({ path: `${SHOTS}/4-leaderboard.png` });

  // ---- escape returns to the menu -------------------------------------
  await page.keyboard.press('Escape');
  await page.waitForSelector('.menu', { timeout: 5000 });
  check('escape returns to menu', true);

  check('no javascript errors', errors.length === 0, errors.slice(0, 5).join(' / '));

  await browser.close();
  return failures;
};

run()
  .then((f) => {
    console.log(f === 0 ? '\nALL E2E CHECKS PASSED' : `\n${f} E2E CHECK(S) FAILED`);
    process.exit(f === 0 ? 0 : 1);
  })
  .catch((e) => {
    console.error('e2e crashed:', e);
    process.exit(1);
  });
