const AUTOR = 'Dayana Gómez';
const CURSO = '';

const W = 1200, H = 800;
const X0 = 50, Y0 = 75, X1 = 400, Y1 = 280;      // lazo principal
const YD = Y1 + 40;                              // cable de descarga
const SW_A = [240, Y1], SW_B = [240, YD], SW_P = [300, Y1];   // interruptor

// Valores por defecto y rangos de cada tipo de componente
const DEF = {
  bat: { val: 5,    min: 0.5, max: 12,    step: 0.5, name: 'Batería',   plural: 'baterías',    pre: 'B', qty: 'Voltaje' },
  res: { val: 1000, min: 10,  max: 10000, step: 10,  name: 'Resistor',  plural: 'resistores',  pre: 'R', qty: 'Resistencia' },
  cap: { val: 1000, min: 100, max: 4700,  step: 100, name: 'Capacitor', plural: 'capacitores', pre: 'C', qty: 'Capacitancia' },
};

// Zonas del circuito donde se sueltan los componentes.
// axis 'h': los componentes van a lo largo de x y las ramas paralelas se desplazan en y.
// axis 'v': al revés. dir indica hacia qué lado crecen las ramas paralelas.
const zones = {
  res: { type: 'res', axis: 'h', base: Y0, a: 110, b: 320, dir: +1, sp: 42, maxRows: 3, maxPer: 4, rows: [] },
  bat: { type: 'bat', axis: 'h', base: Y1, a: 90,  b: 205, dir: -1, sp: 36, maxRows: 3, maxPer: 3, rows: [] },
  cap: { type: 'cap', axis: 'v', base: X1, a: 115, b: 245, dir: +1, sp: 56, maxRows: 3, maxPer: 3, rows: [] },
};

// Recorrido de la corriente (para los puntos animados)
const PATH_C = [[X0, Y0], [X1, Y0], [X1, Y1], [X0, Y1], [X0, Y0]];
const PATH_D = [[X0, Y0], [X1, Y0], [X1, Y1], [SW_P[0], Y1], [SW_B[0], YD], [X0, YD], [X0, Y0]];

// Paleta de componentes (dibujada en el canvas)
const PAL = {
  bat:   { x: 35,  y: 552, w: 92,  h: 78 },
  res:   { x: 137, y: 552, w: 92,  h: 78 },
  cap:   { x: 239, y: 552, w: 92,  h: 78 },
  trash: { x: 351, y: 552, w: 104, h: 78 },
};

// Materiales del conductor: resistividad ρ (Ω·m) y color del cable
const MATS = {
  'Cobre':      { rho: 1.68e-8, col: '#c46a2b' },
  'Aluminio':   { rho: 2.65e-8, col: '#9aa3ad' },
  'Hierro':     { rho: 9.71e-8, col: '#5b5f66' },
  'Constantán': { rho: 4.9e-7,  col: '#c9a227' },
  'Nicromo':    { rho: 1.10e-6, col: '#8a6d5a' },
};

// Multímetro
const MET = { x: 560, y: 50, w: 220, h: 280 };
const MODES = [['V', 'Voltaje'], ['A', 'Corriente'], ['Ω', 'Resistencia'], ['F', 'Capacitancia'], ['Q', 'Carga']];
const JACK_R = [592, 300], JACK_B = [632, 300];
const PROBE_HOME = [[700, 296], [736, 296]];
let meterMode = 'V';
const probe = { target: null };

// Registro de datos (tabla)
const wire = { len: 1, area: 1 };
let rec = [], recStep = 1, nextRec = 0, lastRecMode = 'abierto';

// Colores del estilo caricatura retro (animación de los años 30)
const INK = '#2b1d14', PAPER = '#fbf3df', GLOVE = '#fdf8ec';

// ---------- Modo juego ----------
let appState = 'menu';           // 'menu' | 'map' | 'free' | 'play'
let level = 0, lvl = null, attempts = 0, hintUsed = false, modal = null;
let prog = { unlocked: 1, stars: [] };
let btnCheck, btnHint, btnRetry, btnMap, btnHome, answerInp;
const simEls = [];               // controles que solo se ven dentro del simulador
const MENU_BTNS = { juego: { x: 420, y: 410, w: 360, h: 70 }, libre: { x: 420, y: 500, w: 360, h: 70 } };
const MAP_BACK = { x: 520, y: 640, w: 160, h: 50 };
const MB = {   // botones de las ventanas (intro / victoria)
  ok: { x: 510, y: 515, w: 180, h: 50 },
  repetir: { x: 290, y: 515, w: 190, h: 50 }, mapa: { x: 505, y: 515, w: 190, h: 50 }, sig: { x: 720, y: 515, w: 190, h: 50 },
};

const ctrl = {};
let cnv, btnStyle, btnDel, btnCsv, selMat, T;
let cartoon = false;
let mode = 'abierto';                       // 'carga' | 'descarga' | 'abierto'
let t = 0, vc = 0, I = 0, flow = 0, settled = false;
let hist = [];                              // historial {t, v, q, m}
let drag = null, sel = null;
let toast = { msg: '', until: 0 };
const view = { v: { x: 1, y: 1 }, q: { x: 1, y: 1 } };   // escala animada de cada gráfica

// =====================================================================
function setup() {
  cnv = createCanvas(W, H);
  cnv.parent('app');
  cnv.position(0, 0);

  zones.bat.rows = [[newComp('bat')]];
  zones.res.rows = [[newComp('res')]];
  zones.cap.rows = [[newComp('cap')]];

  simEls.push(
    createButton('Cargar').parent('app').position(30, 478).mousePressed(() => mode = 'carga'),
    createButton('Descargar').parent('app').position(115, 478).mousePressed(() => mode = 'descarga'),
    createButton('Abrir circuito').parent('app').position(230, 478).mousePressed(() => mode = 'abierto'),
    createButton('Reiniciar').parent('app').position(365, 478).mousePressed(reiniciar));

  // botones del modo juego
  btnCheck = createButton('✔ ¡Comprobar!').parent('app').position(450, 8).mousePressed(checkLevel);
  btnHint  = createButton('💡 Pista').parent('app').position(592, 8).mousePressed(() => { hintUsed = true; say('💡 ' + lvl.pista, 9000); });
  btnRetry = createButton('↺ Reiniciar nivel').parent('app').position(688, 8).mousePressed(() => loadLevel(level, false));
  btnMap   = createButton('🗺 Niveles').parent('app').position(880, 8).mousePressed(() => { appState = 'map'; modal = null; refreshUI(); });
  btnHome  = createButton('🏠 Menú').parent('app').position(900, 10).mousePressed(goMenu);
  answerInp = createInput('').parent('app').position(250, 29).size(70);
  answerInp.attribute('placeholder', 'respuesta');
  answerInp.elt.addEventListener('keydown', e => { if (e.key === 'Enter') checkLevel(); });
  btnStyle = createButton('').parent('app').position(1020, 10).mousePressed(() => setCartoon(!cartoon));
  btnDel = createButton('🗑 Eliminar').parent('app').position(1088, 504).mousePressed(deleteSelected);
  btnCsv = createButton('⬇ Descargar CSV').parent('app').position(574, 752).mousePressed(downloadCSV);

  // Ajustes (columna derecha)
  addSlider('sel', 'Valor', 815, 562, 0, 1, 0, 1, v => v);
  addInput('sel');
  ctrl.sel.s.input(() => { if (sel) { sel.val = Number(ctrl.sel.s.value()); setInp('sel', sel.val); } });
  ctrl.sel.inp.changed(() => {
    if (!sel) return;
    const v = parseNum(ctrl.sel.inp.value()), L = LIM[sel.type];
    if (!(v >= L[0] && v <= L[1])) { say('Valor no válido: usa entre ' + L[0] + ' y ' + L[1] + ' ' + UNIT[sel.type]); setInp('sel', sel.val); return; }
    sel.val = v; ctrl.sel.s.value(v); setInp('sel', v);
  });

  selMat = createSelect().parent('app').position(965, 622);
  for (const m in MATS) selMat.option(m);
  selMat.selected('Cobre');
  addSlider('len',  'Longitud',            815, 662, 0.1, 100, 1, 0.1,  v => (+v).toFixed(1) + ' m');
  addSlider('area', 'Sección transversal', 815, 694, 0.05, 4, 1, 0.05, v => (+v).toFixed(2) + ' mm²');
  for (const [k, lim] of [['len', [0.001, 10000]], ['area', [0.001, 100]]]) {
    addInput(k);
    setInp(k, wire[k]);
    ctrl[k].s.input(() => { wire[k] = Number(ctrl[k].s.value()); setInp(k, wire[k]); });
    ctrl[k].inp.changed(() => {
      const v = parseNum(ctrl[k].inp.value());
      if (!(v >= lim[0] && v <= lim[1])) { say('Valor no válido: usa entre ' + lim[0] + ' y ' + lim[1]); setInp(k, wire[k]); return; }
      wire[k] = v; ctrl[k].s.value(v); setInp(k, v);
    });
  }
  addSlider('sp',   'Velocidad (τ/s)',     815, 742, 0.1, 5, 1, 0.1,   v => (+v).toFixed(1) + ' τ/s');

  probe.target = zones.cap.rows[0][0];      // como javalab: el medidor empieza en el capacitor
  simEls.push(btnCsv, btnDel, selMat);
  for (const k of ['sel', 'len', 'area', 'sp']) { simEls.push(ctrl[k].s); if (ctrl[k].inp) simEls.push(ctrl[k].inp); }
  loadProg();

  setCartoon(false);          // siempre empieza en estilo clásico
  selectComp(null);
  refreshUI();
}

function addSlider(key, label, x, y, mn, mx, val, st, fmt) {
  const s = createSlider(mn, mx, val, st);
  s.parent('app');
  s.position(x + 150, y - 4);
  s.style('width', '140px');
  ctrl[key] = { s, label, x, y, fmt };
}

// Casilla para escribir el valor a mano (junto al slider)
function addInput(key) {
  const c = ctrl[key];
  c.s.style('width', '105px');
  c.inp = createInput('').parent('app').position(c.x + 262, c.y - 6).size(58);
  c.inp.attribute('inputmode', 'decimal');
  c.inp.attribute('title', 'Escribe el valor y pulsa Enter');
  c.inp.elt.addEventListener('keydown', e => { if (e.key === 'Enter') c.inp.elt.blur(); });
}
function setInp(key, v) { ctrl[key].inp.value(+(+v).toPrecision(4)); }

// Límites y unidades de lo que se puede escribir para cada componente
const LIM  = { bat: [0.01, 1000], res: [0.1, 1e7], cap: [0.001, 1e6] };
const UNIT = { bat: 'V', res: 'Ω', cap: 'µF' };

// Acepta "4.7", "4,7", "4.7k", "2M", "470m", "1e3"
function parseNum(str) {
  const s = String(str).trim().replace(/\s/g, '').replace(',', '.');
  const m = s.match(/^([0-9]*\.?[0-9]+(?:[eE][-+]?[0-9]+)?)([kKMmuµ]?)/);
  if (!m) return NaN;
  const mult = { '': 1, k: 1e3, K: 1e3, M: 1e6, m: 1e-3, u: 1e-6, 'µ': 1e-6 }[m[2]];
  return parseFloat(m[1]) * mult;
}

function newComp(type) {
  const v = (appState === 'play' && lvl && lvl.nuevo && lvl.nuevo[type]) || DEF[type].val;
  return { type, val: v, name: '' };
}

function setCartoon(on) {
  cartoon = on;
  document.body.classList.toggle('caricatura', on);
  btnStyle.html(on ? '📐 Estilo clásico' : '🎨 Estilo caricatura');
  T = theme();
}

function theme() {
  return cartoon ? {
    bg: color('#ecdcb6'), text: color(INK), sub: color('#5a4632'), wire: color(INK), ww: 4,
    grid: color('#e6d6b0'), charge: color('#3f8f5a'), dis: color('#c8463d'),
    ref: color('#6b5640'), tau: color('#b8a07a'), font: 'Fredoka',
  } : {
    bg: color(248), text: color(30), sub: color(90), wire: color(60), ww: 3,
    grid: color(235), charge: color(30, 160, 80), dis: color(210, 50, 50),
    ref: color(120), tau: color(222, 210, 245), font: 'Arial',
  };
}

function reiniciar() {
  t = 0; vc = 0; I = 0; hist = []; mode = 'abierto'; settled = false;
  rec = []; nextRec = 0; lastRecMode = 'abierto';
}

function downloadCSV() {
  if (!rec.length) { say('Todavía no hay datos: pulsa "Cargar"'); return; }
  const f = v => (+v.toPrecision(6)).toString().replace('.', ',');
  let csv = 'n;modo;t (s);Vc (V);Q (C);I (A)\n';
  rec.forEach((r, i) => { csv += [i + 1, r.m, f(r.t), f(r.v), f(r.q), f(r.i)].join(';') + '\n'; });
  const url = URL.createObjectURL(new Blob(['﻿' + csv], { type: 'text/csv;charset=utf-8' }));
  const a = document.createElement('a');
  a.href = url; a.download = 'datos_circuito_RC.csv';
  document.body.appendChild(a); a.click(); a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}

function selectComp(c) {
  sel = c;
  const s = ctrl.sel.s;
  if (!c) { s.hide(); ctrl.sel.inp.hide(); btnDel.hide(); return; }
  const d = DEF[c.type];
  s.attribute('min', d.min); s.attribute('max', d.max); s.attribute('step', d.step);
  s.value(c.val);
  setInp('sel', c.val);
  if (canEdit(c.type) && !c.hidden) { s.show(); ctrl.sel.inp.show(); } else { s.hide(); ctrl.sel.inp.hide(); }
  if (canAdd(c.type)) btnDel.show(); else btnDel.hide();
}

function deleteSelected() {
  if (!sel || !canAdd(sel.type)) return;
  const z = zones[sel.type];
  const rest = rowsWithout(z, sel);
  if (!rest.length) { say('El circuito necesita al menos un(a) ' + DEF[sel.type].name.toLowerCase()); return; }
  z.rows = rest;
  if (probe.target === sel) probe.target = null;
  selectComp(null);
  say('Componente eliminado');
}

function say(msg, ms = 2600) { toast = { msg, until: millis() + ms }; }

// =====================================================================
//  Física
// =====================================================================
function equivalents() {
  // Resistores: en serie se suman; las ramas en paralelo -> 1/R = Σ 1/R_rama
  let g = 0;
  for (const row of zones.res.rows) g += 1 / row.reduce((a, c) => a + c.val, 0);
  const Rc = 1 / g;

  // Capacitores: en serie 1/C = Σ 1/Ci ; las ramas en paralelo se suman
  let C = 0;
  for (const row of zones.cap.rows) {
    let inv = 0;
    for (const c of row) inv += 1 / (c.val * 1e-6);
    C += 1 / inv;
  }

  // Baterías: en serie se suman los voltajes. Ramas en paralelo: teorema de Millman
  // suponiendo igual resistencia interna en cada batería -> V = Σ(Vi/ni) / Σ(1/ni)
  let num = 0, den = 0;
  for (const row of zones.bat.rows) {
    const v = row.reduce((a, c) => a + c.val, 0), n = row.length;
    num += v / n; den += 1 / n;
  }
  const Vs = num / den;

  // Conductor (alambre): R = ρ·L / A
  const mat = MATS[selMat.value()] || MATS['Cobre'];
  const Rw = mat.rho * wire.len / (wire.area * 1e-6);

  const R = Rc + Rw;
  return { Vs, R, Rc, Rw, C, tau: R * C, mat };
}

function update(P) {
  recStep = niceStep(P.tau / 2);
  if (mode === 'abierto') { I = 0; lastRecMode = 'abierto'; return; }
  const target = mode === 'carga' ? P.Vs : 0;
  if (mode !== lastRecMode) {                 // empieza una carga o descarga: primer dato
    addRec(t, vc, target, P);
    nextRec = t + recStep;
    lastRecMode = mode;
  }
  const eps = 1e-3 * Math.max(P.Vs, 1e-6);
  if (Math.abs(vc - target) < eps) {          // ya llegó al valor final
    vc = target; I = 0; settled = true; return;
  }
  settled = false;
  // La velocidad se mide en "τ por segundo", así cualquier circuito se ve bien
  const dt = Math.min(deltaTime, 50) / 1000 * ctrl.sp.s.value() * P.tau;
  const nsub = 3;
  for (let i = 0; i < nsub; i++) {
    const h = dt / nsub, t0 = t, v0 = vc;
    vc = target + (vc - target) * Math.exp(-h / P.tau);  // solución exacta de la EDO RC
    t += h;
    hist.push({ t, v: vc, q: vc * P.C, m: mode });
    // datos de la tabla a intervalos regulares (valor exacto en ese instante)
    while (nextRec <= t) {
      addRec(nextRec, target + (v0 - target) * Math.exp(-(nextRec - t0) / P.tau), target, P);
      nextRec += recStep;
    }
  }
  I = (target - vc) / P.R;
  if (hist.length > 4000) hist = hist.filter((_, i) => i % 2 === 0);
}

function addRec(tt, v, target, P) {
  rec.push({ m: mode, t: tt, v, q: v * P.C, i: (target - v) / P.R });
  if (rec.length > 3000) rec.shift();
}

// Lo que mide el multímetro en un componente (o en el alambre)
function measure(tg, P) {
  if (!tg) return null;
  if (tg === 'wire') return { V: I * P.Rw, I, R: P.Rw, C: null, Q: null };
  const z = zones[tg.type];
  const row = z.rows.find(r => r.includes(tg));
  if (!row) return null;
  if (tg.type === 'res') {
    const Rrow = row.reduce((a, c) => a + c.val, 0);
    const Ir = I * P.Rc / Rrow;                 // corriente de esa rama
    return { V: Ir * tg.val, I: Ir, R: tg.val, C: null, Q: null };
  }
  if (tg.type === 'cap') {
    let inv = 0;
    for (const c of row) inv += 1 / (c.val * 1e-6);
    const Crow = 1 / inv, Ci = tg.val * 1e-6, Q = Crow * vc;
    return { V: Q / Ci, I: I * Crow / P.C, R: Infinity, C: Ci, Q };
  }
  // batería ideal
  let den = 0;
  for (const r of z.rows) den += 1 / r.length;
  const Ib = mode === 'carga' ? I * (1 / row.length) / den : 0;
  return { V: tg.val, I: Ib, R: 0, C: null, Q: null };
}

// =====================================================================
//  Dibujo principal
// =====================================================================
function draw() {
  textFont(T.font);
  if (appState === 'menu' || appState === 'map') {
    drawBackground();
    if (appState === 'menu') drawMenu(); else drawMap();
    drawToast();
    filmFX();
    const bs = appState === 'menu' ? Object.values(MENU_BTNS) : [MAP_BACK, ...LEVELS.map((_, i) => mapTile(i))];
    cursor(bs.some(inBox) ? HAND : ARROW);
    return;
  }
  drawBackground();
  nameComps();
  const P = equivalents();
  update(P);

  drawTopBar();

  if (probe.target && probe.target !== 'wire' &&
      !zones[probe.target.type].rows.some(r => r.includes(probe.target))) probe.target = null;

  drawCircuit(P);
  drawReadouts(P);
  drawMeter(P);
  drawTable();
  plot(810, 60, 370, 175, 'Voltaje del capacitor Vc(t)', 'V', 'v', P.Vs, P);
  plot(810, 300, 370, 175, 'Carga del capacitor Q(t)', 'C', 'q', P.Vs * P.C, P);
  drawLeads();
  drawPalette();
  drawSettings(P);
  drawDragged();
  drawToast();
  drawModal();
  filmFX();
  if (modal) cursor(Object.values(MB).some(inBox) ? HAND : ARROW); else updateCursor();
}

function drawBackground() {
  background(T.bg);
  if (!cartoon) return;
  // fibras del papel (fijas)
  randomSeed(7); noStroke();
  for (let i = 0; i < 160; i++) { fill(120, 90, 50, 14); ellipse(random(W), random(H), random(20, 90), random(6, 26)); }
  randomSeed(millis());
}

// Efecto de película antigua: viñeta, grano, rayones y parpadeo
function filmFX() {
  if (!cartoon) return;
  const g = drawingContext.createRadialGradient(W / 2, H / 2, H * 0.35, W / 2, H / 2, H * 0.9);
  g.addColorStop(0, 'rgba(70,40,10,0)'); g.addColorStop(1, 'rgba(70,40,10,0.30)');
  drawingContext.fillStyle = g; drawingContext.fillRect(0, 0, W, H);
  noStroke(); fill(43, 29, 20, 30);
  for (let i = 0; i < 380; i++) rect(random(W), random(H), random(1, 2.4), random(1, 2.4));
  if (random() < 0.12) { stroke(43, 29, 20, 45); strokeWeight(1); const x = random(W); line(x, 0, x + random(-25, 25), H); }
  noStroke(); fill(255, 245, 215, random(0, 16)); rect(0, 0, W, H);
}

// Ojo "de pastel" (pie-cut eye) típico de la animación de los años 30
function pieEye(x, y, w, h, a, look = 0) {
  stroke(ca(INK, a)); strokeWeight(1.3); fill(ca(GLOVE, a)); ellipse(x, y, w, h);
  noStroke(); fill(ca(INK, a));
  const px = x + look * w * 0.12, py = y + h * 0.08;
  arc(px, py, w * 0.55, h * 0.72, -0.75, TWO_PI - 1.65, PIE);
}

// Guante blanco con brazo de manguera (rubber hose)
function hoseArm(x1, y1, x2, y2, bend, a) {
  noFill(); stroke(ca(INK, a)); strokeWeight(2.4);
  bezier(x1, y1, x1 + bend * 0.4, y1 + 7, x2 - bend * 0.3, y2 + 7, x2, y2);
  stroke(ca(INK, a)); strokeWeight(1.3); fill(ca(GLOVE, a)); circle(x2, y2, 7.5);
  line(x2 - 1.5, y2 - 3.5, x2 - 1.5, y2 - 1); line(x2 + 1.2, y2 - 3.5, x2 + 1.2, y2 - 1);
}

// Línea normal o "temblorosa" de dibujo animado
function wl(x1, y1, x2, y2) {
  if (!cartoon) { line(x1, y1, x2, y2); return; }
  const L = dist(x1, y1, x2, y2);
  if (L < 1) return;
  const n = Math.max(2, Math.floor(L / 16));
  const nx = -(y2 - y1) / L, ny = (x2 - x1) / L;
  const b = Math.floor(frameCount / 14) * 0.37;      // "hervor" de línea, como en caricaturas
  noFill();
  beginShape();
  for (let i = 0; i <= n; i++) {
    const u = i / n, px = lerp(x1, x2, u), py = lerp(y1, y2, u);
    const o = (i === 0 || i === n) ? 0 : (noise(px * 0.04, py * 0.04, b) - 0.5) * 3.2;
    vertex(px + nx * o, py + ny * o);
  }
  endShape();
}

function panel(x, y, w, h, r) {
  if (cartoon) {
    r = r ?? 16;
    noStroke(); fill(43, 29, 20, 200); rect(x + 5, y + 5, w, h, r);
    fill(PAPER); stroke(INK); strokeWeight(3); rect(x, y, w, h, r);
  } else {
    r = r ?? 8;
    fill(255); stroke(180); strokeWeight(1); rect(x, y, w, h, r);
  }
}

function ca(c, a) { const k = color(c); k.setAlpha(a); return k; }

// =====================================================================
//  Zonas y disposición de componentes
// =====================================================================
function zp(z, along, off) { return z.axis === 'h' ? [along, z.base + off] : [z.base + off, along]; }

function half(type) {
  if (type === 'res') return 18;
  if (type === 'bat') return cartoon ? 18 : 5;
  return cartoon ? 12 : 6;
}

function layoutZone(z, rows) {
  const items = [], wires = [], nodes = [];
  const L = z.b - z.a;
  rows.forEach((row, r) => {
    const off = r * z.sp * z.dir;
    if (r > 0) {
      const prevOff = (r - 1) * z.sp * z.dir;
      wires.push([...zp(z, z.a, prevOff), ...zp(z, z.a, off)]);
      wires.push([...zp(z, z.b, prevOff), ...zp(z, z.b, off)]);
      nodes.push(zp(z, z.a, prevOff), zp(z, z.b, prevOff));
    }
    const seg = L / row.length;
    let prev = z.a;
    row.forEach((c, i) => {
      const ctr = z.a + seg * (i + 0.5), h = half(c.type);
      wires.push([...zp(z, prev, off), ...zp(z, ctr - h, off)]);
      prev = ctr + h;
      const [x, y] = zp(z, ctr, off);
      items.push({ comp: c, x, y, row: r, idx: i });
    });
    wires.push([...zp(z, prev, off), ...zp(z, z.b, off)]);
  });
  return { items, wires, nodes };
}

function rowsWithout(z, comp) {
  return z.rows.map(r => r.filter(c => c !== comp)).filter(r => r.length);
}

function zoneBox(z) {
  const p1 = -22, p2 = (z.maxRows - 1) * z.sp + 22;
  const lo = Math.min(p1 * z.dir, p2 * z.dir);
  const a = z.a - 22, b = z.b + 22, th = p2 - p1;
  return z.axis === 'h' ? [a, z.base + lo, b - a, th] : [z.base + lo, a, th, b - a];
}

// Calcula dónde quedaría el componente si se suelta en (mx, my)
function computeTarget(comp, mx, my) {
  const z = zones[comp.type];
  const [bx, by, bw, bh] = zoneBox(z);
  if (mx < bx || mx > bx + bw || my < by || my > by + bh) return null;
  const base = rowsWithout(z, comp);
  const along = z.axis === 'h' ? mx : my;
  const d = ((z.axis === 'h' ? my : mx) - z.base) * z.dir;
  let r = constrain(Math.round(d / z.sp), 0, base.length);
  if (r === base.length && r >= z.maxRows) r = base.length - 1;
  if (r < base.length && base[r].length >= z.maxPer) {
    if (base.length < z.maxRows) r = base.length;     // la fila está llena: nueva rama
    else return { rows: null };                        // no cabe
  }
  const rows = base.map(x => x.slice());
  if (r === rows.length) rows.push([comp]);           // nueva rama -> paralelo
  else {                                              // misma línea -> serie
    const row = rows[r], seg = (z.b - z.a) / row.length;
    let i = 0;
    while (i < row.length && z.a + seg * (i + 0.5) < along) i++;
    row.splice(i, 0, comp);
  }
  return { rows };
}

function rowsForDraw(z) {
  if (!drag || !drag.comp || !drag.moved || drag.comp.type !== z.type) return z.rows;
  if (drag.target && drag.target.rows) return drag.target.rows;
  if (!drag.isNew && overTrash()) {
    const w = rowsWithout(z, drag.comp);
    if (w.length) return w;
  }
  return z.rows;
}

function nameComps() {
  for (const k in zones) {
    let n = 0;
    for (const row of zones[k].rows) for (const c of row) c.name = DEF[k].pre + (++n);
  }
}

function describe(z) {
  const r = z.rows, n = r.reduce((a, x) => a + x.length, 0);
  if (n === 1) return '1';
  if (r.length === 1) return n + ' en serie';
  if (r.every(x => x.length === 1)) return n + ' en paralelo';
  return n + ' mixto';
}

// =====================================================================
//  Circuito
// =====================================================================
function fixedWires() {
  return [
    [X0, Y0, X0, Y1], [X0, Y1, X0, YD], [X0, YD, SW_B[0], YD],
    [X0, Y0, zones.res.a, Y0], [zones.res.b, Y0, X1, Y0],
    [X1, Y0, X1, zones.cap.a], [X1, zones.cap.b, X1, Y1],
    [X0, Y1, zones.bat.a, Y1], [zones.bat.b, Y1, SW_A[0], Y1], [SW_P[0], Y1, X1, Y1],
  ];
}

// Cables con el color del material del conductor
function drawWires(list, P) {
  strokeCap(ROUND); noFill();
  if (cartoon) {
    push(); translate(1.6, 1.1); stroke(200, 70, 60, 70); strokeWeight(7); for (const w of list) wl(...w); pop();
    push(); translate(-1.6, -1.1); stroke(60, 150, 170, 70); strokeWeight(7); for (const w of list) wl(...w); pop();
    stroke(INK); strokeWeight(7.5); for (const w of list) wl(...w);
    stroke(P.mat.col); strokeWeight(3.2); for (const w of list) wl(...w);
  } else {
    stroke(lerpColor(color(60), color(P.mat.col), 0.6)); strokeWeight(3);
    for (const w of list) wl(...w);
  }
}

function drawCircuit(P) {
  drawWires(fixedWires(), P);
  noStroke(); fill(T.wire); circle(X0, Y1, cartoon ? 9 : 7);

  // resaltar el alambre si el multímetro lo está midiendo o se va a soltar ahí
  const wireHot = probe.target === 'wire' || (drag && drag.kind === 'probe' && drag.hover === 'wire');
  if (wireHot) {
    stroke(255, 200, 0, 120); strokeWeight(12); line(X0, Y0 + 20, X0, Y1 - 20);
  }

  if (drag && drag.comp && drag.moved) zoneHighlight(zones[drag.comp.type]);

  const frac = constrain(vc / Math.max(P.Vs, 1e-6), 0, 1);
  busy = constrain(Math.abs(I) * P.R / Math.max(P.Vs, 1e-6), 0, 1);
  for (const k of ['res', 'bat', 'cap']) {
    const z = zones[k];
    const L = layoutZone(z, rowsForDraw(z));
    drawWires(L.wires, P);
    noStroke(); fill(T.wire);
    for (const n of L.nodes) circle(n[0], n[1], cartoon ? 9 : 7);
    for (const it of L.items) {
      const ghost = drag && drag.moved && it.comp === drag.comp;
      if (drag && drag.kind === 'probe' && drag.hover === it.comp) {
        noStroke(); fill(255, 200, 0, 110); circle(it.x, it.y, 52);
      }
      drawComp(it.comp, it.x, it.y, z.axis, ghost ? 110 : 255, frac, it.comp === sel && !drag);
      if (!ghost) compLabel(it);
    }
  }

  // Interruptor
  const end = mode === 'carga' ? SW_A : mode === 'descarga' ? SW_B : [275, Y1 - 50];
  if (cartoon) {
    stroke(INK); strokeWeight(10); line(SW_P[0], SW_P[1], end[0], end[1]);
    stroke('#d9822b'); strokeWeight(5); line(SW_P[0], SW_P[1], end[0], end[1]);
    stroke(INK); strokeWeight(1.5); fill(GLOVE); circle(end[0], end[1], 12);
    line(end[0] - 2, end[1] - 5, end[0] - 2, end[1] - 1.5); line(end[0] + 2, end[1] - 5, end[0] + 2, end[1] - 1.5);
  } else {
    stroke(230, 120, 0); strokeWeight(4); line(SW_P[0], SW_P[1], end[0], end[1]);
  }
  stroke(T.wire); strokeWeight(cartoon ? 3 : 2); fill(255);
  for (const p of [SW_A, SW_B, SW_P]) circle(p[0], p[1], cartoon ? 11 : 8);
  noStroke(); fill(T.sub); textSize(12);
  textAlign(CENTER, TOP); text('carga', SW_A[0], Y1 + 10);
  textAlign(LEFT, CENTER); text('descarga', SW_B[0] + 12, YD + 6);

  // Corriente: puntos que se mueven
  if (mode !== 'abierto' && !settled) {
    const r = constrain(I * P.R / Math.max(P.Vs, 1e-6), -1, 1);
    if (Math.abs(r) > 0.003) {
      flow += r * 3;
      const path = mode === 'carga' ? PATH_C : PATH_D;
      const tot = pathLen(path), n = 16;
      if (cartoon) { stroke(INK); strokeWeight(2); fill('#f0cd78'); }
      else { noStroke(); fill(255, 150, 0, 220); }
      for (let i = 0; i < n; i++) {
        const p = pathPoint(path, flow + i * tot / n);
        circle(p[0], p[1], cartoon ? 10 : 7);
      }
    }
  }
}

function zoneHighlight(z) {
  const [x, y, w, h] = zoneBox(z);
  const ok = drag.target && drag.target.rows;
  const bad = drag.target && !drag.target.rows;
  noStroke();
  fill(ok ? color(60, 200, 120, 50) : bad ? color(255, 80, 80, 50) : color(255, 255, 255, 70));
  rect(x, y, w, h, 10);
  stroke(ok ? color(30, 160, 80) : bad ? color(220, 50, 50) : color(120));
  strokeWeight(2); noFill();
  drawingContext.setLineDash([6, 5]); rect(x, y, w, h, 10); drawingContext.setLineDash([]);
}

function compLabel(it) {
  const c = it.comp;
  noStroke(); fill(T.sub); textSize(11);
  if (c.type === 'res') {
    textAlign(CENTER, BASELINE); text(c.hidden ? '¿ ? Ω' : si(c.val, 'Ω'), it.x, it.y - 13);
  } else if (c.type === 'bat') {
    textAlign(CENTER, TOP); text(c.val.toFixed(1) + ' V', it.x, it.y + (cartoon ? 13 : 15));
  } else {
    textAlign(LEFT, CENTER); textSize(10);
    const lx = it.x + (cartoon ? 18 : 16); text(c.val, lx, it.y - 5); text('µF', lx, it.y + 6);
  }
}

// ---------- Componentes ----------
function drawComp(c, x, y, axis, a, frac, selected, scl = 1) {
  push();
  translate(x, y);
  if (axis === 'v') rotate(HALF_PI);
  scale(scl);
  if (selected) {
    noStroke(); fill(cartoon ? color(255, 255, 255, 210) : color(80, 140, 255, 55));
    rect(-25, -20, 50, 40, 12);
    stroke(cartoon ? INK : color(80, 140, 255)); strokeWeight(2); noFill();
    drawingContext.setLineDash([4, 4]); rect(-25, -20, 50, 40, 12); drawingContext.setLineDash([]);
  }
  const ink = cartoon ? ca(INK, a) : color(60, a);
  strokeCap(ROUND);
  if (c.type === 'res') drawRes(c, ink, a);
  else if (c.type === 'bat') drawBat(ink, a);
  else drawCap(ink, a, frac);
  pop();
}

let busy = 0;   // fracción de corriente (para las caras)
function drawRes(c, ink, a) {
  if (!cartoon) {
    stroke(ink); strokeWeight(3); noFill();
    beginShape(); vertex(-18, 0);
    for (let k = 0; k < 6; k++) vertex(-18 + 36 * (k + 0.5) / 6, k % 2 ? 8 : -8);
    vertex(18, 0); endShape();
    return;
  }
  // resistor con cara: se esfuerza (cejas y boca) cuando pasa corriente
  const wave = sin(frameCount * 0.12 + c.val) * 2;
  hoseArm(-13, 3, -21, wave * 0.3, -6, a);
  hoseArm(13, 3, 21, -wave * 0.3, 6, a);
  stroke(ink); strokeWeight(3); fill(ca('#e2c08d', a)); rect(-15, -10, 30, 20, 9);
  noStroke();
  (c.hidden ? ['#9e9e9e', '#9e9e9e', '#9e9e9e'] : bands(c.val)).forEach((col, i) => { fill(ca(col, a)); rect(2 + i * 3.6, -9, 2.4, 18); });
  fill(ca('#b8892a', a)); rect(13, -8, 1.5, 16);
  pieEye(-9, -3, 5.5, 7.5, a, busy);
  pieEye(-3.5, -3, 5.5, 7.5, a, busy);
  stroke(ink); strokeWeight(1.4); noFill();
  if (busy > 0.15) { line(-12, -8.5, -7.5, -7); line(-1, -8.5, -5, -7); line(-9, 4.5, -3.5, 4.5); }
  else arc(-6.2, 3, 7, 4.5, 0, PI);
}

// Código de colores real del resistor (2 cifras + multiplicador)
function bands(v) {
  const C = ['#111111', '#8b4513', '#e53935', '#fb8c00', '#fdd835', '#43a047', '#1e88e5', '#8e24aa', '#9e9e9e', '#ffffff'];
  let e = Math.floor(Math.log10(v)) - 1;
  let d = Math.round(v / Math.pow(10, e));
  if (d >= 100) { d = 10; e += 1; }
  return [C[Math.floor(d / 10)], C[d % 10], C[constrain(e, 0, 9)]];
}

function drawBat(ink, a) {
  if (!cartoon) {
    stroke(ink); strokeCap(SQUARE); strokeWeight(4);
    line(-5, -13, -5, 13);       // placa larga (+)
    line(4, -7, 4, 7);           // placa corta (−)
    noStroke(); fill(ink); textSize(11); textAlign(CENTER, CENTER); text('+', -13, -10);
    return;
  }
  // pila con cara y guantes que sostienen los cables
  const wave = sin(frameCount * 0.15) * 1.5;
  hoseArm(-15, 4, -22, wave, -5, a);
  hoseArm(16, 4, 23, -wave, 5, a);
  stroke(ink); strokeWeight(3);
  fill(ca('#c9c0ad', a)); rect(-20, -4, 5, 8, 2);                 // borne +
  fill(ca('#c8463d', a)); rect(-16, -11, 11, 22, 6, 0, 0, 6);
  fill(ca('#e8b84a', a)); rect(-5, -11, 22, 22, 0, 6, 6, 0);
  stroke(ca(GLOVE, a)); strokeWeight(2); line(-13.5, 0, -7.5, 0); line(-10.5, -3, -10.5, 3);
  pieEye(2.5, -3, 5.5, 8, a, 0.6);
  pieEye(9, -3, 5.5, 8, a, 0.6);
  noFill(); stroke(ink); strokeWeight(1.5); arc(6, 3, 9, 6, 0.1, PI - 0.1);   // sonrisa grande
  noStroke(); fill(ca('#e9a79c', a)); ellipse(0, 4, 3, 2); ellipse(12.5, 4, 3, 2);
}

function drawCap(ink, a, frac) {
  if (!cartoon) {
    strokeCap(SQUARE); strokeWeight(6);
    const c1 = lerpColor(color(150), color(220, 50, 50), frac); c1.setAlpha(a);
    const c2 = lerpColor(color(150), color(50, 90, 220), frac); c2.setAlpha(a);
    stroke(c1); line(-6, -14, -6, 14);
    stroke(c2); line(6, -14, 6, 14);
    // cargas acumuladas en las placas (como en javalab)
    const n = Math.round(frac * 4);
    noStroke();
    for (let i = 0; i < n; i++) {
      const yy = -10.5 + i * 7;
      fill(220, 50, 50, a); circle(-12, yy, 3.5);
      fill(50, 90, 220, a); circle(12, yy, 3.5);
    }
    return;
  }
  const c1 = lerpColor(color(255), color('#c8463d'), frac); c1.setAlpha(a);
  const c2 = lerpColor(color(255), color('#4f8fa8'), frac); c2.setAlpha(a);
  // placas con cara: los ojos en la placa (+) y la boca en la (−); sonríe al cargarse
  stroke(ink); strokeWeight(3);
  fill(c1); rect(-12, -15, 8, 30, 4);
  fill(c2); rect(4, -15, 8, 30, 4);
  pieEye(-8, -6, 6, 5, a, 0);
  pieEye(-8, 6, 6, 5, a, 0);
  noFill(); stroke(ink); strokeWeight(1.5);
  if (frac > 0.5) arc(6, 0, 6, 12, -HALF_PI, HALF_PI);
  else line(8, -4, 8, 4);
  // signos de carga
  const n = Math.round(frac * 2), ys = [-9, 9];
  strokeWeight(2);
  for (let i = 0; i < n; i++) {
    const yy = ys[i];
    stroke(ca('#a8322a', a)); line(-19, yy, -15, yy); line(-17, yy - 2, -17, yy + 2);
    stroke(ca('#3b6e8f', a)); line(15, yy, 19, yy);
  }
}

// ---------- Recorrido de la corriente ----------
function pathLen(pts) {
  let s = 0;
  for (let i = 1; i < pts.length; i++) s += dist(pts[i - 1][0], pts[i - 1][1], pts[i][0], pts[i][1]);
  return s;
}

function pathPoint(pts, d) {
  const tot = pathLen(pts);
  d = ((d % tot) + tot) % tot;
  for (let i = 1; i < pts.length; i++) {
    const a = pts[i - 1], b = pts[i], L = dist(a[0], a[1], b[0], b[1]);
    if (d <= L) return [lerp(a[0], b[0], d / L), lerp(a[1], b[1], d / L)];
    d -= L;
  }
  return pts[0];
}

// =====================================================================
//  Lecturas
// =====================================================================
function drawReadouts(P) {
  panel(30, 336, 440, 132);
  noStroke(); fill(T.text); textAlign(LEFT, BASELINE);
  textSize(14); textStyle(BOLD);
  text('Interruptor: ' + mode + (settled && mode !== 'abierto' ? '  ✓ estable' : ''), 45, 357);
  textAlign(RIGHT, BASELINE); text('t = ' + si(t, 's'), 455, 357);
  textStyle(NORMAL); textSize(13); textAlign(LEFT, BASELINE);
  const x1 = 45, x2 = 262;
  text('V equivalente: ' + si(P.Vs, 'V'), x1, 380);
  text('Voltaje capacitor: ' + si(vc, 'V'), x2, 380);
  text('R total (con alambre): ' + si(P.R, 'Ω'), x1, 400);
  text('Carga Q: ' + si(vc * P.C, 'C'), x2, 400);
  text('C equivalente: ' + si(P.C, 'F'), x1, 420);
  text('Corriente: ' + si(I, 'A'), x2, 420);
  text('τ = R·C: ' + si(P.tau, 's'), x1, 440);
  text('V resistencias: ' + si(I * P.R, 'V'), x2, 440);
  fill(T.sub); textSize(11);
  text('Baterías: ' + describe(zones.bat) + '   ·   Resistores: ' + describe(zones.res) +
       '   ·   Capacitores: ' + describe(zones.cap), x1, 460);
}

// =====================================================================
//  Gráficas con escala automática
// =====================================================================
function niceStep(raw) {
  if (!(raw > 0)) return 1;
  const e = Math.pow(10, Math.floor(Math.log10(raw))), f = raw / e;
  const k = 1.001, m = f <= k ? 1 : f <= 2 * k ? 2 : f <= 2.5 * k ? 2.5 : f <= 5 * k ? 5 : 10;
  return m * e;
}
function niceCeil(v) { const s = niceStep(v / 5); return Math.ceil(v / s) * s; }

function plot(x, y, w, h, title, unit, key, ref, P) {
  const vw = view[key];
  let dmax = 0;
  for (const p of hist) if (p[key] > dmax) dmax = p[key];
  const tEnd = hist.length ? hist[hist.length - 1].t : 0;

  // Escalas objetivo: siguen a los datos reales (pequeños o grandes)
  const yT = hist.length ? niceCeil(Math.max(dmax * 1.15, ref * 0.02, 1e-12)) : niceCeil(ref * 1.1 || 1);
  const xT = hist.length ? Math.max(tEnd * 1.08, P.tau * 0.5) : 5 * P.tau;
  if (vw.y / yT > 50 || yT / vw.y > 50) vw.y = yT;
  if (vw.x / xT > 50 || xT / vw.x > 50) vw.x = xT;
  vw.y = Math.max(lerp(vw.y, yT, 0.12), dmax * 1.02, 1e-15);
  vw.x = Math.max(lerp(vw.x, xT, 0.15), tEnd * 1.01, 1e-9);

  const toX = tt => map(tt, 0, vw.x, x, x + w);
  const toY = v => map(v, 0, vw.y, y + h, y);

  panel(x, y, w, h, cartoon ? 12 : 0);

  // Rejilla y valores del eje Y
  const sy = niceStep(vw.y / 4);
  textSize(10);
  for (let k = 0; k * sy <= vw.y * 1.0001; k++) {
    const gy = toY(k * sy);
    if (k > 0) { stroke(T.grid); strokeWeight(1); line(x + 2, gy, x + w - 2, gy); }
    if (gy - 12 > y) { noStroke(); fill(T.sub); textAlign(LEFT, BOTTOM); text(si(k * sy, unit), x + 4, gy - 1); }
  }
  // Rejilla y valores del eje X (tiempo)
  const sx = niceStep(vw.x / 5);
  for (let k = 0; k * sx <= vw.x * 1.0001; k++) {
    const gx = toX(k * sx);
    if (k > 0) { stroke(T.grid); strokeWeight(1); line(gx, y + 2, gx, y + h - 2); }
    if (gx < x + w - 10) { noStroke(); fill(T.sub); textAlign(CENTER, TOP); text(si(k * sx, 's'), gx, y + h + 3); }
  }
  // Marcas de τ, 2τ, 3τ...
  if (P.tau / vw.x * w > 30) {
    stroke(T.tau); strokeWeight(1); drawingContext.setLineDash([3, 4]);
    for (let k = 1; k <= 10 && k * P.tau < vw.x; k++) line(toX(k * P.tau), y + 14, toX(k * P.tau), y + h);
    drawingContext.setLineDash([]);
    noStroke(); fill(150, 120, 220); textSize(9); textAlign(CENTER, TOP);
    for (let k = 1; k <= 10 && k * P.tau < vw.x; k++) text(k === 1 ? 'τ' : k + 'τ', toX(k * P.tau), y + 3);
  }
  // Valor final
  if (ref > 0) {
    if (ref <= vw.y) {
      const ry = toY(ref);
      stroke(T.ref); strokeWeight(1.2); drawingContext.setLineDash([6, 5]);
      line(x + 2, ry, x + w - 2, ry); drawingContext.setLineDash([]);
      noStroke(); fill(T.sub); textSize(10); textAlign(RIGHT, BOTTOM);
      text('final: ' + si(ref, unit), x + w - 4, ry - 2);
    } else {
      noStroke(); fill(T.sub); textSize(10); textAlign(RIGHT, TOP);
      text('↑ final: ' + si(ref, unit), x + w - 4, y + 14);
    }
  }

  // Curva
  const passes = cartoon ? [[color(INK), 7], [null, 4]] : [[null, 2.5]];
  noFill(); strokeJoin(ROUND);
  for (const [col, wt] of passes) {
    strokeWeight(wt);
    let i = 0;
    while (i < hist.length) {
      const m = hist[i].m;
      stroke(col || (m === 'carga' ? T.charge : T.dis));
      beginShape();
      if (i > 0) vertex(toX(hist[i - 1].t), toY(hist[i - 1][key]));
      while (i < hist.length && hist[i].m === m) { vertex(toX(hist[i].t), toY(hist[i][key])); i++; }
      endShape();
    }
  }

  // Valor actual en la punta de la curva
  if (hist.length) {
    const p = hist[hist.length - 1], px = toX(p.t), py = toY(p[key]);
    const col = p.m === 'carga' ? T.charge : T.dis;
    stroke(cartoon ? INK : 255); strokeWeight(2); fill(col); circle(px, py, cartoon ? 11 : 8);
    const lbl = si(p[key], unit);
    textSize(11);
    const tw = textWidth(lbl) + 10;
    let lx = px + 8;
    if (lx + tw > x + w - 2) lx = px - 8 - tw;
    const ly = constrain(py - 24, y + 2, y + h - 20);
    fill(255); stroke(col); strokeWeight(cartoon ? 2 : 1); rect(lx, ly, tw, 17, 6);
    noStroke(); fill(T.text); textAlign(LEFT, CENTER); text(lbl, lx + 5, ly + 9);
  } else {
    noStroke(); fill(T.sub); textSize(12); textAlign(CENTER, CENTER);
    text('Pulsa "Cargar" para empezar', x + w / 2, y + h / 2);
  }

  // Título y leyenda
  noStroke(); fill(T.text); textSize(cartoon ? 15 : 14); textStyle(BOLD); textAlign(LEFT, BASELINE);
  text(title, x, y - 9); textStyle(NORMAL);
  textSize(11); textAlign(RIGHT, BASELINE);
  fill(T.dis); text('● descarga', x + w, y - 9);
  fill(T.charge); text('● carga', x + w - 72, y - 9);
}

// =====================================================================
//  Paleta, ajustes y arrastre
// =====================================================================
function inBox(b) { return mouseX >= b.x && mouseX <= b.x + b.w && mouseY >= b.y && mouseY <= b.y + b.h; }
function overTrash() { return mouseX >= 20 && mouseX <= 470 && mouseY >= 515 && mouseY <= 790; }

function tile(b, hot, hotCol) {
  if (cartoon) {
    noStroke(); fill(INK); rect(b.x + 3, b.y + 3, b.w, b.h, 12);
    stroke(INK); strokeWeight(3); fill(hot ? (hotCol || '#f0cd78') : '#f7ecd2'); rect(b.x, b.y, b.w, b.h, 12);
  } else {
    stroke(hot ? 120 : 200); strokeWeight(1); fill(hot ? (hotCol || 235) : 250); rect(b.x, b.y, b.w, b.h, 8);
  }
}

function drawPalette() {
  panel(20, 515, 450, 275);
  noStroke(); fill(T.text); textAlign(LEFT, BASELINE); textSize(14); textStyle(BOLD);
  text('Arrastra componentes al circuito', 35, 540); textStyle(NORMAL);

  for (const k of ['bat', 'res', 'cap']) {
    const b = PAL[k];
    const ok = canAdd(k);
    tile(b, ok && inBox(b) && !drag);
    drawComp({ type: k, val: DEF[k].val }, b.x + b.w / 2, b.y + 30, k === 'cap' ? 'v' : 'h', ok ? 255 : 90, 0.6, false, 1.15);
    noStroke(); fill(T.text); textAlign(CENTER, BASELINE); textSize(12);
    const nv = appState === 'play' && lvl.nuevo && lvl.nuevo[k];
    text(DEF[k].name + (ok && nv ? ' ' + fmtVal({ type: k, val: nv }) : ''), b.x + b.w / 2, b.y + b.h - 8);
    if (!ok) { textSize(22); textAlign(CENTER, CENTER); text('🔒', b.x + b.w - 16, b.y + 16); }
  }

  const tb = PAL.trash, hot = drag && drag.moved && !drag.isNew && overTrash();
  tile(tb, hot, cartoon ? '#e9a79c' : color(255, 225, 225));
  drawTrash(tb.x + tb.w / 2, tb.y + 28, hot);
  noStroke(); fill(T.text); textAlign(CENTER, BASELINE); textSize(12);
  text('Suelta aquí', tb.x + tb.w / 2, tb.y + tb.h - 20);
  text('para quitar', tb.x + tb.w / 2, tb.y + tb.h - 7);

  fill(T.sub); textAlign(LEFT, BASELINE); textSize(12);
  text('• En la misma línea que otro componente → quedan en SERIE', 35, 655);
  text('• En una línea nueva (rama) → quedan en PARALELO', 35, 674);
  text('• Clic en un componente para cambiar su valor (Supr lo elimina)', 35, 693);
  text('• Arrastra las pinzas del multímetro para medir V, A, Ω, F o Q', 35, 712);
  text('• Cambia material, longitud y grosor del alambre en "Conductor"', 35, 731);
  text('• Los datos de la tabla se pueden descargar en CSV (Excel)', 35, 750);
}

function drawTrash(x, y, hot) {
  push(); translate(x, y);
  if (hot) rotate(sin(frameCount * 0.3) * 0.12);
  stroke(cartoon ? INK : 90); strokeWeight(cartoon ? 3 : 2);
  fill(cartoon ? '#a9c7c9' : 230);
  rect(-11, -6, 22, 22, 3);
  line(-15, -10, 15, -10); line(-4, -14, 4, -14);
  line(-4, -2, -4, 12); line(4, -2, 4, 12);
  pop();
}

function drawSettings(P) {
  const x = 800, y = 500;
  panel(x, y, 380, 290);
  noStroke(); fill(T.text); textAlign(LEFT, BASELINE); textSize(14); textStyle(BOLD);
  text('Seleccionado: ' + (sel ? sel.name + ' (' + DEF[sel.type].name.toLowerCase() + ')' : 'ninguno'), x + 15, y + 22);
  textStyle(NORMAL); textSize(11); fill(T.sub);
  if (sel) {
    text('Escribe el valor en la casilla y pulsa Enter' + (sel.type === 'res' ? ' (acepta 4.7k, 2M)' : ''), x + 15, y + 40);
    const c = ctrl.sel;
    fill(T.text); textSize(13); textAlign(LEFT, CENTER);
    text(DEF[sel.type].qty, c.x, c.y + 6);
    if (canEdit(sel.type) && !sel.hidden) text(UNIT[sel.type], c.x + 330, c.y + 6);
    else { fill(T.sub); text(sel.hidden ? '🔒 ¿?  (¡mídelo!)' : fmtVal(sel) + '   🔒 fijo en este nivel', c.x + 150, c.y + 6); }
  } else {
    text('Haz clic en un componente del circuito para escribir su valor.', x + 15, y + 40);
  }

  stroke(cartoon ? INK : 225); strokeWeight(cartoon ? 2 : 1); line(x + 15, y + 85, x + 365, y + 85);
  noStroke(); fill(T.text); textSize(14); textStyle(BOLD); textAlign(LEFT, BASELINE);
  text('Conductor (alambre)' + (canEdit('wire') ? '' : '  🔒'), x + 15, y + 108);
  textAlign(RIGHT, BASELINE); text('R = ' + si(P.Rw, 'Ω'), x + 365, y + 108);
  textStyle(NORMAL); textSize(13); textAlign(LEFT, CENTER);
  text('Material', x + 15, 633);
  textSize(11); fill(T.sub); text('ρ = ' + si(P.mat.rho, 'Ω·m'), x + 290, 633);
  fill(T.text); textSize(13);
  for (const k of ['len', 'area', 'sp']) {
    const c = ctrl[k];
    text(c.label, c.x, c.y + 6);
    if (k === 'sp') text(c.fmt(c.s.value()), c.x + 300, c.y + 6);
    else text(k === 'len' ? 'm' : 'mm²', c.x + 330, c.y + 6);
  }
  stroke(cartoon ? INK : 225); strokeWeight(cartoon ? 2 : 1); line(x + 15, 718, x + 365, 718);
  noStroke(); fill(T.sub); textSize(10.5); textAlign(LEFT, BASELINE);
  text('R = ρ·L/A  ·  Medidas ideales (el multímetro no altera el circuito)', x + 15, 780);
}

function fmtVal(c) {
  if (c.type === 'bat') return (+c.val).toFixed(1) + ' V';
  if (c.type === 'res') return si(c.val, 'Ω');
  return c.val + ' µF';
}

// =====================================================================
//  Multímetro
// =====================================================================
function modeBtn(i) { return { x: MET.x + 15 + i * 39, y: MET.y + 108, w: 34, h: 28 }; }

function targetName(tg) {
  if (tg === 'wire') return 'alambre (conductor)';
  return tg.name + ' (' + DEF[tg.type].name.toLowerCase() + ')';
}

function meterText(m) {
  if (!m) return ['- - - -', 'sin conectar'];
  switch (meterMode) {
    case 'V': return [si(m.V, 'V'), ''];
    case 'A': return [si(m.I, 'A'), ''];
    case 'Ω': return m.R === Infinity ? ['OL', 'circuito abierto'] : m.R === 0 ? ['0 Ω', 'batería ideal'] : [si(m.R, 'Ω'), ''];
    case 'F': return m.C != null ? [si(m.C, 'F'), ''] : ['- - - -', 'solo capacitores'];
    default:  return m.Q != null ? [si(m.Q, 'C'), ''] : ['- - - -', 'solo capacitores'];
  }
}

function drawMeter(P) {
  const m = MET;
  panel(m.x, m.y, m.w, m.h);
  noStroke(); fill(T.text); textAlign(LEFT, BASELINE); textSize(15); textStyle(BOLD);
  text('Multímetro', m.x + 15, m.y + 24); textStyle(NORMAL);

  // pantalla
  const lx = m.x + 15, ly = m.y + 34, lw = m.w - 30, lh = 62;
  if (cartoon) { stroke(INK); strokeWeight(3); fill('#dfe3bd'); rect(lx, ly, lw, lh, 10); }
  else { stroke(110); strokeWeight(1); fill('#d7e4c4'); rect(lx, ly, lw, lh, 4); }
  const [val, note] = meterText(measure(probe.target, P));
  const mName = MODES.find(x => x[0] === meterMode)[1];
  noStroke(); fill(25);
  textFont(cartoon ? 'Fredoka' : 'Courier New'); textStyle(BOLD); textSize(25); textAlign(RIGHT, CENTER);
  text(val, lx + lw - 10, ly + 25);
  textStyle(NORMAL); textFont(T.font); textSize(10);
  textAlign(LEFT, BOTTOM); text(mName, lx + 8, ly + lh - 5);
  textAlign(RIGHT, BOTTOM); text(note, lx + lw - 8, ly + lh - 5);

  // botones de modo
  MODES.forEach(([k], i) => {
    const b = modeBtn(i), on = k === meterMode, hov = inBox(b) && !drag;
    if (cartoon) {
      noStroke(); fill(INK); rect(b.x + 2, b.y + 2, b.w, b.h, 8);
      stroke(INK); strokeWeight(2.5); fill(on ? '#c8463d' : hov ? '#f0cd78' : '#e8b84a'); rect(b.x, b.y, b.w, b.h, 8);
    } else {
      stroke(on ? color(40, 90, 200) : 170); strokeWeight(1);
      fill(on ? color(60, 110, 220) : hov ? 235 : 248); rect(b.x, b.y, b.w, b.h, 4);
    }
    noStroke(); fill(on && !cartoon ? 255 : 20); textSize(14); textStyle(BOLD); textAlign(CENTER, CENTER);
    text(k, b.x + b.w / 2, b.y + b.h / 2 + 1); textStyle(NORMAL);
  });

  noStroke(); fill(T.text); textSize(12); textAlign(LEFT, BASELINE);
  text('Midiendo: ' + (probe.target ? targetName(probe.target) : 'nada'), m.x + 15, m.y + 160);
  fill(T.sub); textSize(10.5);
  text('Arrastra las pinzas a un componente', m.x + 15, m.y + 178);
  text('o al alambre para medirlo.', m.x + 15, m.y + 192);

  // bornes
  stroke(cartoon ? INK : 80); strokeWeight(cartoon ? 3 : 1.5);
  fill('#e03131'); circle(JACK_R[0], JACK_R[1], 14);
  fill(40); circle(JACK_B[0], JACK_B[1], 14);
  noStroke(); fill(T.sub); textSize(9); textAlign(CENTER, BOTTOM);
  text('+', JACK_R[0], JACK_R[1] - 9); text('COM', JACK_B[0], JACK_B[1] - 9);
}

function probeEnds() {
  if (drag && drag.kind === 'probe') return [[mouseX - 7, mouseY], [mouseX + 7, mouseY]];
  if (probe.target) {
    const e = targetEnds(probe.target);
    if (e) return e;
  }
  return PROBE_HOME;
}

function targetEnds(tg) {
  if (tg === 'wire') return [[X0, Y0 + 30], [X0, Y1 - 30]];
  const z = zones[tg.type];
  const it = layoutZone(z, z.rows).items.find(i => i.comp === tg);
  if (!it) return null;
  const h = half(tg.type) + 6;
  return z.axis === 'h' ? [[it.x - h, it.y], [it.x + h, it.y]] : [[it.x, it.y - h], [it.x, it.y + h]];
}

function drawLeads() {
  const ends = probeEnds(), home = ends === PROBE_HOME;
  const jacks = [JACK_R, JACK_B], cols = [color('#e03131'), color(45)];
  for (let i = 0; i < 2; i++) {
    const j = jacks[i], e = ends[i];
    const c1 = [j[0] - 20, j[1] + 35], c2 = [e[0], e[1] + (home ? 30 : 70)];
    noFill();
    if (cartoon) { stroke(INK); strokeWeight(7); bezier(j[0], j[1], ...c1, ...c2, e[0], e[1]); }
    stroke(cols[i]); strokeWeight(cartoon ? 3.5 : 2.5);
    bezier(j[0], j[1], ...c1, ...c2, e[0], e[1]);
    // pinza
    stroke(cartoon ? INK : 30); strokeWeight(cartoon ? 2.5 : 1); fill(cols[i]);
    rect(e[0] - 5, e[1] - 6, 10, 12, 3);
  }
}

function hitWire(mx, my) {
  for (const [x1, y1, x2, y2] of fixedWires()) {
    const L2 = (x2 - x1) ** 2 + (y2 - y1) ** 2;
    const u = constrain(((mx - x1) * (x2 - x1) + (my - y1) * (y2 - y1)) / L2, 0, 1);
    if (dist(mx, my, x1 + u * (x2 - x1), y1 + u * (y2 - y1)) < 8) return true;
  }
  return false;
}

// =====================================================================
//  Tabla de datos
// =====================================================================
function drawTable() {
  const x = 560, y = 345, w = 220, h = 445;
  panel(x, y, w, h);
  noStroke(); fill(T.text); textAlign(LEFT, BASELINE); textSize(14); textStyle(BOLD);
  text('Registro de datos', x + 12, y + 22); textStyle(NORMAL);
  fill(T.sub); textSize(10.5);
  text('Un dato cada ' + si(recStep, 's'), x + 12, y + 39);
  textAlign(RIGHT, BASELINE); text(rec.length + ' datos', x + w - 12, y + 39);

  const cx = [x + 24, x + 88, x + 152];
  fill(T.text); textSize(11); textStyle(BOLD); textAlign(LEFT, BASELINE);
  text('Tiempo', cx[0], y + 60); text('Voltaje', cx[1], y + 60); text('Carga', cx[2], y + 60);
  textStyle(NORMAL);
  stroke(cartoon ? INK : 210); strokeWeight(cartoon ? 2 : 1); line(x + 10, y + 67, x + w - 10, y + 67);

  const maxRows = 18, start = Math.max(0, rec.length - maxRows);
  noStroke(); textSize(11);
  for (let j = 0; start + j < rec.length; j++) {
    const r = rec[start + j], yy = y + 84 + j * 17;
    if (j % 2 === 0) { fill(cartoon ? color(255, 236, 160, 120) : color(245)); rect(x + 8, yy - 12, w - 16, 17, 3); }
    fill(r.m === 'carga' ? T.charge : T.dis); circle(x + 15, yy - 4, 7);
    fill(T.text);
    text(si(r.t, 's'), cx[0], yy); text(si(r.v, 'V'), cx[1], yy); text(si(r.q, 'C'), cx[2], yy);
  }
  if (!rec.length) {
    fill(T.sub); textAlign(CENTER, CENTER); textSize(12);
    text('Sin datos todavía.', x + w / 2, y + 180);
    text('Pulsa "Cargar" para registrar.', x + w / 2, y + 198);
  }
}

// =====================================================================
//  Arrastre de componentes y pinzas
// =====================================================================
function drawDragged() {
  if (!drag || !drag.moved || !drag.comp) return;
  const z = zones[drag.comp.type];
  drawComp(drag.comp, mouseX + 3, mouseY + 4, z.axis, 60, 0, false, 1.2);   // sombra
  drawComp(drag.comp, mouseX, mouseY, z.axis, 255, 0.6, false, 1.2);
  if (drag.target && !drag.target.rows) {
    noStroke(); fill(220, 50, 50); textSize(12); textAlign(LEFT, CENTER);
    text('✕ no cabe aquí', mouseX + 22, mouseY - 18);
  }
}

function drawToast() {
  if (millis() > toast.until) return;
  textSize(13);
  const full = textWidth(toast.msg), tw = Math.min(full + 30, 440);
  const lines = Math.ceil(full / (tw - 30) + 0.05), th = lines * 18 + 14;
  const x = (appState === 'menu' || appState === 'map' ? W / 2 : 245) - tw / 2, y = appState === 'menu' || appState === 'map' ? 600 : 160;
  if (cartoon) { noStroke(); fill(INK); rect(x + 3, y + 3, tw, th, 15); stroke(INK); strokeWeight(3); fill('#e8b84a'); }
  else { noStroke(); fill(40, 40, 40, 225); }
  rect(x, y, tw, th, 15);
  noStroke(); fill(cartoon ? INK : 255); textAlign(CENTER, CENTER); text(toast.msg, x + 15, y + 4, tw - 30, th - 8);
}

function hitComp(mx, my) {
  for (const k in zones) {
    const z = zones[k];
    for (const it of layoutZone(z, z.rows).items) {
      const dx = Math.abs(mx - it.x), dy = Math.abs(my - it.y);
      const [da, dp] = z.axis === 'h' ? [dx, dy] : [dy, dx];
      if (da < 20 && dp < 16) return it.comp;
    }
  }
  return null;
}

function overProbe() { return probeEnds().some(e => dist(mouseX, mouseY, e[0], e[1]) < 12); }

function updateCursor() {
  if (drag && drag.moved) cursor('grabbing');
  else if (inBox(PAL.bat) || inBox(PAL.res) || inBox(PAL.cap) || overProbe() || hitComp(mouseX, mouseY)) cursor('grab');
  else if (MODES.some((_, i) => inBox(modeBtn(i)))) cursor(HAND);
  else cursor(ARROW);
}

// ---------- Ratón ----------
function mousePressed(e) {
  if (e && e.target !== cnv.elt) return;          // clic en un botón o slider
  if (appState === 'menu') { menuClick(); return false; }
  if (appState === 'map') { mapClick(); return false; }
  if (modal) { modalClick(); return false; }
  for (let i = 0; i < MODES.length; i++) {
    if (inBox(modeBtn(i))) { meterMode = MODES[i][0]; return false; }
  }
  if (overProbe()) { drag = { kind: 'probe', moved: true, hover: probe.target }; return false; }
  for (const k of ['bat', 'res', 'cap']) {
    if (inBox(PAL[k])) {
      if (!canAdd(k)) { say('🔒 En este nivel no puedes agregar ' + DEF[k].plural); return false; }
      drag = { comp: newComp(k), isNew: true, moved: true, target: null }; return false;
    }
  }
  const c = hitComp(mouseX, mouseY);
  if (c) { drag = { comp: c, isNew: false, moved: false, sx: mouseX, sy: mouseY, target: null, noMove: !canAdd(c.type) }; return false; }
  if (mouseX < 540 && mouseY < 330) selectComp(null);
}

function mouseDragged() {
  if (!drag) return;
  if (drag.kind === 'probe') {
    drag.hover = hitComp(mouseX, mouseY) || (hitWire(mouseX, mouseY) ? 'wire' : null);
    return false;
  }
  if (drag.noMove) return false;
  if (!drag.moved && dist(mouseX, mouseY, drag.sx, drag.sy) > 5) drag.moved = true;
  if (drag.moved) drag.target = computeTarget(drag.comp, mouseX, mouseY);
  return false;
}

function mouseReleased() {
  if (!drag) return;
  const d = drag;
  drag = null;
  if (d.kind === 'probe') { probe.target = d.hover; return; }
  const z = zones[d.comp.type];
  if (!d.moved) {                                                   // fue un clic
    if (canEdit(z.type) || canAdd(z.type)) selectComp(d.comp);
    else say('🔒 Este componente está bloqueado en este nivel');
    return;
  }
  if (d.isNew && appState === 'play' && lvl.max && lvl.max[z.type] &&
      z.rows.flat().length >= lvl.max[z.type]) { say('Solo puedes usar ' + lvl.max[z.type] + ' ' + DEF[z.type].plural); return; }
  if (d.target && d.target.rows) { z.rows = d.target.rows; selectComp(d.comp); return; }
  if (d.target && !d.target.rows) { say('No caben más ' + DEF[d.comp.type].plural + ' en esa zona'); return; }
  if (!d.isNew && overTrash()) {
    const w = rowsWithout(z, d.comp);
    if (!w.length) say('El circuito necesita al menos un(a) ' + DEF[d.comp.type].name.toLowerCase());
    else {
      z.rows = w;
      if (sel === d.comp) selectComp(null);
      if (probe.target === d.comp) probe.target = null;
      say('Componente eliminado');
    }
  } else if (d.isNew) {
    say('Suelta el componente en la zona resaltada');
  }
}

function keyPressed() {
  const tag = document.activeElement && document.activeElement.tagName;
  if (tag === 'SELECT' || tag === 'INPUT') return;
  if ((keyCode === DELETE || keyCode === BACKSPACE) && sel && !modal) deleteSelected();
}

// =====================================================================
//  MODO JUEGO
// =====================================================================
function canEdit(type) { return appState !== 'play' || !lvl || lvl.editar.includes(type); }
function canAdd(type)  { return appState !== 'play' || !lvl || lvl.agregar.includes(type); }

// El progreso NO se guarda: cada vez que se abre la página empieza desde el nivel 1
function loadProg() {
  prog = { unlocked: 1, stars: [] };
  while (prog.stars.length < LEVELS.length) prog.stars.push(0);
}
function saveProg() {}

function setWire(mat, len, area) {
  selMat.selected(mat);
  wire.len = len; wire.area = area;
  ctrl.len.s.value(len); ctrl.area.s.value(area);
  setInp('len', len); setInp('area', area);
}

function buildCircuit(ini) {
  for (const k of ['bat', 'res', 'cap']) zones[k].rows = ini[k].map(r => r.map(v => ({ type: k, val: v, name: '' })));
}

// Muestra u oculta los controles según la pantalla y los bloqueos del nivel
function refreshUI() {
  const inSim = (appState === 'free' || appState === 'play') && !modal;
  for (const e of simEls) inSim ? e.show() : e.hide();
  modal ? btnStyle.hide() : btnStyle.show();
  for (const b of [btnCheck, btnHint, btnRetry, btnMap]) (appState === 'play' && !modal) ? b.show() : b.hide();
  (appState === 'free' && !modal) ? btnHome.show() : btnHome.hide();
  (appState === 'play' && !modal && lvl.respuesta) ? answerInp.show() : answerInp.hide();
  const wl = !canEdit('wire');
  for (const e of [selMat, ctrl.len.s, ctrl.len.inp, ctrl.area.s, ctrl.area.inp]) wl ? e.attribute('disabled', '') : e.removeAttribute('disabled');
  if (inSim) selectComp(sel && zones[sel.type].rows.some(r => r.includes(sel)) ? sel : null);
}

function goMenu() { appState = 'menu'; modal = null; refreshUI(); }

function startFree() {
  appState = 'free'; lvl = null; modal = null;
  buildCircuit({ bat: [[5]], res: [[1000]], cap: [[1000]] });
  setWire('Cobre', 1, 1);
  reiniciar();
  probe.target = zones.cap.rows[0][0]; meterMode = 'V';
  sel = null;
  refreshUI();
}

function loadLevel(i, intro = true) {
  appState = 'play';
  if (intro || i !== level) { attempts = 0; hintUsed = false; }
  level = i; lvl = LEVELS[i];
  buildCircuit(lvl.inicial);
  const a = lvl.alambre || ['Cobre', 1, 1];
  setWire(a[0], a[1], a[2]);
  reiniciar();
  if (lvl.setup) lvl.setup(lvl);
  probe.target = zones.cap.rows[0][0]; meterMode = 'V';
  answerInp.value('');
  sel = null; drag = null;
  modal = intro ? { type: 'intro' } : null;
  refreshUI();
}

function checkLevel() {
  if (appState !== 'play' || modal) return;
  attempts++;
  const P = equivalents();
  const g = { P, vc, Q: vc * P.C, settled, mode, hist, answer: parseNum(answerInp.value()) };
  const [ok, msg] = lvl.objetivo(g);
  if (!ok) { say('🔋 Todavía no: ' + msg, 5500); return; }
  const stars = Math.max(1, 3 - (hintUsed ? 1 : 0) - (attempts > 2 ? 1 : 0));
  prog.stars[level] = Math.max(prog.stars[level] || 0, stars);
  prog.unlocked = Math.max(prog.unlocked, Math.min(LEVELS.length, level + 2));
  saveProg();
  modal = { type: 'win', stars, t0: millis() };
  refreshUI();
}

function describeAllowed() {
  const nom = { bat: 'el voltaje de la batería', res: 'el valor de los resistores', cap: 'el valor de los capacitores', wire: 'el alambre (material, largo y grosor)' };
  const p = lvl.editar.map(k => nom[k]).concat(lvl.agregar.map(k => 'agregar o quitar ' + DEF[k].plural));
  return p.length ? 'Puedes cambiar: ' + p.join(', ') + '.' : 'No puedes cambiar componentes: ¡observa y mide!';
}

// ---------- Dibujo de pantallas ----------
function titleText(str, x, y, size, align = LEFT) {
  noStroke(); textAlign(align, BASELINE);
  if (cartoon) {
    textFont('Rye'); textSize(size);
    fill(200, 70, 60, 150); text(str, x + 2, y + 2);
    fill(60, 150, 170, 150); text(str, x - 2, y - 2);
    fill(INK); text(str, x, y);
    textFont(T.font);
  } else {
    fill(T.text); textSize(size); textStyle(BOLD); text(str, x, y); textStyle(NORMAL);
  }
}

function drawTopBar() {
  if (appState === 'play') {
    titleText('Nivel ' + (level + 1) + '/' + LEVELS.length + ' · ' + lvl.titulo, 30, 25, cartoon ? 17 : 17);
    noStroke(); fill(T.text); textSize(13); textAlign(LEFT, BASELINE);
    text('🎯 ' + lvl.meta, 30, 46);
    if (lvl.respuesta) text(lvl.respuesta.unidad, 326, 46);
  } else {
    titleText('Simulador de carga y descarga de un capacitor (circuito RC)', 30, 34, cartoon ? 21 : 20);
  }
}

function cbtn(b, label, primary) {
  const hov = inBox(b), o = hov ? -1 : 0;
  if (cartoon) {
    noStroke(); fill(43, 29, 20, 220); rect(b.x + 4, b.y + 4, b.w, b.h, 16);
    stroke(INK); strokeWeight(3);
    fill(primary ? (hov ? '#d4584f' : '#c8463d') : (hov ? '#f0cd78' : '#e8b84a'));
    rect(b.x + o, b.y + o, b.w, b.h, 16);
  } else {
    stroke(primary ? color(40, 90, 200) : 160); strokeWeight(1);
    fill(primary ? (hov ? color(70, 120, 230) : color(60, 110, 220)) : (hov ? 232 : 250));
    rect(b.x, b.y, b.w, b.h, 8);
  }
  noStroke(); fill(primary ? (cartoon ? GLOVE : 255) : T.text);
  textSize(b.h > 60 ? 24 : 17); textStyle(BOLD); textAlign(CENTER, CENTER);
  text(label, b.x + b.w / 2 + o, b.y + b.h / 2 + 1 + o); textStyle(NORMAL);
}

function starShape(x, y, r, on, a = 255) {
  push(); translate(x, y);
  stroke(cartoon ? color(INK) : color(120, 90, 0)); strokeWeight(cartoon ? 3 : 1.5);
  fill(on ? color(232, 184, 74, a) : color(200, 190, 170, a));
  beginShape();
  for (let i = 0; i < 10; i++) { const ang = -HALF_PI + i * PI / 5, rr = i % 2 ? r * 0.45 : r; vertex(Math.cos(ang) * rr, Math.sin(ang) * rr); }
  endShape(CLOSE);
  pop();
}

function totalStars() { return prog.stars.reduce((a, b) => a + b, 0); }

function drawMenu() {
  titleText('Circuito RC', W / 2, 150, 60, CENTER);
  noStroke(); fill(T.sub); textSize(20); textAlign(CENTER, BASELINE);
  text('Carga y descarga de un capacitor', W / 2, 192);
  textSize(17); textStyle(BOLD); fill(T.text);
  text('Hecho por: ' + AUTOR, W / 2, 226);
  textStyle(NORMAL);
  if (CURSO) { textSize(14); fill(T.sub); text(CURSO, W / 2, 248); }
  busy = 0;
  const f = (Math.sin(frameCount * 0.03) + 1) / 2;
  drawComp({ type: 'bat', val: 5 }, W / 2 - 170, 300, 'h', 255, f, false, 3);
  drawComp({ type: 'res', val: 1000 }, W / 2, 300, 'h', 255, f, false, 3);
  drawComp({ type: 'cap', val: 1000 }, W / 2 + 170, 300, 'v', 255, f, false, 2.6);
  cbtn(MENU_BTNS.juego, '🎮  Modo juego', true);
  cbtn(MENU_BTNS.libre, '🔬  Modo libre', false);
  noStroke(); fill(T.sub); textSize(14); textAlign(CENTER, BASELINE);
  text('Progreso: ★ ' + totalStars() + ' / ' + LEVELS.length * 3 + '   ·   Niveles desbloqueados: ' + prog.unlocked + ' / ' + LEVELS.length, W / 2, 615);
  text('Modo juego: resuelve retos cambiando voltajes, resistencias, capacitores y el alambre.', W / 2, 640);
}

function mapTile(i) { return { x: 95 + (i % 5) * 206, y: 190 + Math.floor(i / 5) * 200, w: 186, h: 170 }; }

function drawMap() {
  titleText('Elige un nivel', W / 2, 110, 40, CENTER);
  noStroke(); fill(T.sub); textSize(15); textAlign(CENTER, BASELINE);
  text('★ ' + totalStars() + ' / ' + LEVELS.length * 3 + ' estrellas', W / 2, 145);
  LEVELS.forEach((L, i) => {
    const b = mapTile(i), open = i < prog.unlocked;
    tile(b, open && inBox(b));
    noStroke(); fill(open ? T.text : T.sub); textAlign(CENTER, CENTER);
    if (cartoon) textFont('Rye');
    textSize(40); text(i + 1, b.x + b.w / 2, b.y + 45);
    textFont(T.font);
    textSize(13); textAlign(CENTER, TOP); text(L.titulo, b.x + 10, b.y + 82, b.w - 20, 40);
    for (let k = 0; k < 3; k++) starShape(b.x + b.w / 2 + (k - 1) * 30, b.y + 140, 11, k < (prog.stars[i] || 0));
    if (!open) { fill(cartoon ? color(43, 29, 20, 110) : color(0, 0, 0, 60)); rect(b.x, b.y, b.w, b.h, 12); fill(255); textSize(34); textAlign(CENTER, CENTER); text('🔒', b.x + b.w / 2, b.y + b.h / 2); }
  });
  cbtn(MAP_BACK, '🏠 Menú', false);
}

function drawModal() {
  if (!modal) return;
  noStroke(); fill(30, 20, 10, 150); rect(0, 0, W, H);
  const x = 250, y = 170, w = 700, h = 420;
  panel(x, y, w, h, 20);
  if (modal.type === 'intro') {
    busy = 0;
    drawComp({ type: 'bat', val: 5 }, x + 120, y + 170, 'h', 255, 0, false, 3.2);
    noStroke(); fill(T.sub); textSize(13); textAlign(CENTER, TOP); text('Pila', x + 120, y + 225);
    titleText('Nivel ' + (level + 1) + ': ' + lvl.titulo, x + 240, y + 60, 24);
    noStroke(); fill(T.text); textSize(16); textAlign(LEFT, TOP);
    text('«' + lvl.historia + '»', x + 240, y + 85, w - 270, 150);
    textStyle(BOLD); text('🎯 Objetivo: ' + lvl.meta, x + 240, y + 240, w - 270, 44); textStyle(NORMAL);
    fill(T.sub); textSize(13); text(describeAllowed(), x + 240, y + 285, w - 270, 50);
    cbtn(MB.ok, '¡A jugar!', true);
  } else {
    const last = level === LEVELS.length - 1;
    titleText(last ? '¡Juego completado!' : '¡Nivel superado!', W / 2, y + 65, 32, CENTER);
    const el = millis() - modal.t0;
    for (let k = 0; k < 3; k++) {
      const s = constrain((el - k * 250) / 300, 0, 1), on = k < modal.stars;
      starShape(W / 2 + (k - 1) * 90, y + 145 - (k === 1 ? 12 : 0), (on ? 36 : 30) * (0.4 + 0.6 * s), on);
    }
    noStroke(); fill(T.text); textSize(15); textAlign(CENTER, TOP);
    text('💡 ' + lvl.explica, x + 50, y + 210, w - 100, 60);
    fill(T.sub); textSize(13);
    text(attempts + (attempts === 1 ? ' intento' : ' intentos') + (hintUsed ? ' · usaste pista' : ' · sin pistas') +
      (last ? '   ·   Total: ★ ' + totalStars() + ' / ' + LEVELS.length * 3 : ''), x + 50, y + 280, w - 100, 40);
    cbtn(MB.repetir, '↺ Repetir', false);
    cbtn(MB.mapa, '🗺 Niveles', false);
    cbtn(MB.sig, last ? '🏠 Menú' : 'Siguiente ▶', true);
  }
}

function menuClick() {
  if (inBox(MENU_BTNS.juego)) { appState = 'map'; refreshUI(); }
  else if (inBox(MENU_BTNS.libre)) startFree();
}

function mapClick() {
  if (inBox(MAP_BACK)) { goMenu(); return; }
  for (let i = 0; i < LEVELS.length; i++) {
    if (inBox(mapTile(i))) {
      if (i < prog.unlocked) loadLevel(i, true);
      else say('🔒 Supera el nivel ' + i + ' para desbloquear este.');
      return;
    }
  }
}

function modalClick() {
  if (modal.type === 'intro') {
    if (inBox(MB.ok)) { modal = null; refreshUI(); }
    return;
  }
  if (inBox(MB.repetir)) loadLevel(level, true);
  else if (inBox(MB.mapa)) { modal = null; appState = 'map'; refreshUI(); }
  else if (inBox(MB.sig)) {
    if (level === LEVELS.length - 1) goMenu(); else loadLevel(level + 1, true);
  }
}

// =====================================================================
//  Utilidad: valores con prefijo SI
// =====================================================================
function si(v, u) {
  const a = Math.abs(v);
  if (a === 0) return '0 ' + u;
  const pref = [[1e6, 'M'], [1e3, 'k'], [1, ''], [1e-3, 'm'], [1e-6, 'µ'], [1e-9, 'n']];
  let d = 1e-9, p = 'n';
  for (const [f, s] of pref) if (a >= f * 0.9995) { d = f; p = s; break; }
  return (+(v / d).toPrecision(3)) + ' ' + p + u;
}