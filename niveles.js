
function cerca(v, obj, tol) { return Math.abs(v - obj) <= tol * Math.abs(obj); }

const LEVELS = [
  {
    titulo: '¡Enciende el circuito!',
    meta: 'Carga el capacitor hasta Vc = 6 V',
    historia: '¡Hola! Soy Pila. Mi voltaje es muy bajito y el capacitor tiene hambre de carga. Cambia mi voltaje para que el capacitor llegue a 6 V.',
    inicial: { bat: [[1.5]], res: [[1000]], cap: [[1000]] },
    editar: ['bat'], agregar: [],
    pista: 'Haz clic en la batería, escribe 6 en la casilla de la derecha y pulsa Enter. Luego pulsa "Cargar" y espera a que diga ✓ estable.',
    explica: 'Cuando el capacitor termina de cargarse, su voltaje es igual al de la fuente.',
    objetivo: g => cerca(g.vc, 6, 0.03) ? [true]
      : [false, `Vc es ${si(g.vc, 'V')} y necesitas 6 V. ¿Pulsaste "Cargar" y esperaste?`],
  },
  {
    titulo: 'Guardando carga',
    meta: 'Logra una carga Q = 4,5 mC',
    historia: 'Con mis 9 V el capacitor guarda demasiada carga. Necesito exactamente 4,5 mC. Solo puedes cambiar el capacitor.',
    inicial: { bat: [[9]], res: [[1000]], cap: [[1000]] },
    editar: ['cap'], agregar: [],
    pista: 'Q = C·V, entonces C = Q / V = 4,5 mC / 9 V. Pasa el resultado a µF (1 mF = 1000 µF), escríbelo y carga.',
    explica: 'La carga que guarda un capacitor es Q = C·V: más capacitancia o más voltaje = más carga.',
    objetivo: g => cerca(g.Q, 4.5e-3, 0.03) ? [true]
      : [false, `La carga es ${si(g.Q, 'C')}; necesitas 4,5 mC. Recuerda cargar por completo.`],
  },
  {
    titulo: '¡Más lento!',
    meta: 'Haz que τ sea 5 s',
    historia: 'El capacitor se carga tan rápido que nadie alcanza a verlo. Haz que la constante de tiempo τ sea de 5 segundos cambiando la resistencia.',
    inicial: { bat: [[9]], res: [[1000]], cap: [[1000]] },
    editar: ['res'], agregar: [],
    pista: 'τ = R·C. Con C = 1000 µF = 0,001 F, ¿qué R da 5 s? Puedes escribir "5k" en la casilla.',
    explica: 'τ = R·C: a mayor resistencia, la corriente es menor y el capacitor tarda más en cargarse.',
    objetivo: g => cerca(g.P.tau, 5, 0.03) ? [true]
      : [false, `τ = ${si(g.P.tau, 's')}; necesitas 5 s.`],
  },
  {
    titulo: 'Solo tengo de 1 kΩ',
    meta: 'R equivalente ≈ 333 Ω',
    historia: 'Necesito una resistencia de 333 Ω, pero en la caja solo hay resistores de 1 kΩ y no se pueden cambiar. ¡Combínalos!',
    inicial: { bat: [[9]], res: [[1000]], cap: [[1000]] },
    editar: [], agregar: ['res'], nuevo: { res: 1000 },
    pista: 'En paralelo la resistencia baja: 1/R = 1/R1 + 1/R2 + 1/R3. Suelta cada resistor nuevo en una línea nueva (debajo), no al lado.',
    explica: 'Tres resistencias iguales en paralelo dan R/3: el paralelo abre más caminos para la corriente.',
    objetivo: g => cerca(g.P.Rc, 1000 / 3, 0.02) ? [true]
      : [false, `La R equivalente es ${si(g.P.Rc, 'Ω')}; necesitas unos 333 Ω.`],
  },
  {
    titulo: 'Pilas en fila',
    meta: 'Carga a 13,5 V con pilas de 4,5 V',
    historia: '¡Necesito ayuda de mis hermanas! Cada pila da 4,5 V. Agrega pilas para cargar el capacitor hasta 13,5 V.',
    inicial: { bat: [[4.5]], res: [[1000]], cap: [[1000]] },
    editar: [], agregar: ['bat'], nuevo: { bat: 4.5 },
    pista: 'Las baterías en SERIE suman su voltaje: suelta las pilas nuevas al lado de la primera, en la misma línea. Luego pulsa Cargar.',
    explica: 'En serie los voltajes se suman; en paralelo el voltaje es el mismo (pero duran más).',
    objetivo: g => cerca(g.vc, 13.5, 0.03) ? [true]
      : cerca(g.P.Vs, 13.5, 0.03) ? [false, 'El voltaje de las pilas ya es correcto: ahora pulsa "Cargar" y espera.']
      : [false, `Las pilas dan ${si(g.P.Vs, 'V')}; necesitas 13,5 V.`],
  },
  {
    titulo: 'El resistor misterioso',
    meta: 'Mide R2 (en Ω)',
    historia: 'Alguien borró el valor de R2. Usa el multímetro para descubrir cuánto vale y escribe la respuesta en la casilla de arriba.',
    inicial: { bat: [[12]], res: [[1000, 1000]], cap: [[1000]] },
    editar: [], agregar: [],
    respuesta: { unidad: 'Ω' },
    setup: lv => {
      const op = [1200, 1800, 2200, 2700, 3300, 4700, 5600];
      const r2 = zones.res.rows[0][1];
      r2.val = op[Math.floor(Math.random() * op.length)];
      r2.hidden = true;
      lv._secreto = r2.val;
    },
    pista: 'Arrastra las pinzas del multímetro hasta R2 y pulsa el botón Ω. Escribe ese número en la casilla y pulsa Comprobar.',
    explica: 'El óhmetro mide la resistencia de un componente. También podías calcularla con τ = R·C.',
    objetivo: function (g) {
      if (!(g.answer > 0)) return [false, 'Escribe tu respuesta en la casilla de arriba (puedes usar "2.2k").'];
      return cerca(g.answer, this._secreto, 0.03) ? [true] : [false, `${si(g.answer, 'Ω')} no es correcto. ¡Mide con el multímetro!`];
    },
  },
  {
    titulo: 'Alambre resistente',
    meta: 'El alambre debe tener 500 Ω',
    historia: 'Para este experimento el propio alambre debe hacer de resistencia: 500 Ω. Cambia el material, la longitud o la sección.',
    inicial: { bat: [[9]], res: [[10]], cap: [[1000]] },
    editar: ['wire'], agregar: [],
    pista: 'R = ρ·L/A. El cobre casi no tiene resistencia: prueba con Nicromo (ρ = 1,1 µΩ·m). Con A = 0,1 mm², L ≈ 45 m.',
    explica: 'Un alambre tiene más resistencia si es largo, delgado y de un material con alta resistividad.',
    objetivo: g => cerca(g.P.Rw, 500, 0.05) ? [true]
      : [false, `El alambre tiene ${si(g.P.Rw, 'Ω')}; necesitas 500 Ω (±5 %).`],
  },
  {
    titulo: 'Lector de datos',
    meta: 'Escribe Vc cuando t = 2τ',
    historia: 'Aquí τ = 2 s. Carga el capacitor y averigua su voltaje justo en t = 2τ, usando la tabla de datos o la gráfica.',
    inicial: { bat: [[10]], res: [[2000]], cap: [[1000]] },
    editar: [], agregar: [],
    respuesta: { unidad: 'V' },
    pista: 'Pulsa "Cargar". En la tabla busca la fila t = 4 s (2τ = 2·2 s). Comprueba: Vc = V·(1 − e⁻²) ≈ 0,865·V.',
    explica: 'En t = τ el capacitor llega al 63 % del voltaje final, en 2τ al 86 % y en 5τ a más del 99 %.',
    objetivo: g => {
      if (!(g.answer > 0)) return [false, 'Escribe el voltaje en la casilla de arriba.'];
      const ok = g.P.Vs * (1 - Math.exp(-2));
      return cerca(g.answer, ok, 0.03) ? [true] : [false, `${si(g.answer, 'V')} no coincide. Revisa la fila de t = 4 s en la tabla.`];
    },
  },
  {
    titulo: 'Descarga controlada',
    meta: 'Carga y luego descarga hasta Q < 5 %',
    historia: 'Ahora al revés: llena el capacitor por completo y después descárgalo hasta que le quede menos del 5 % de su carga.',
    inicial: { bat: [[9]], res: [[1000]], cap: [[2200]] },
    editar: [], agregar: [],
    pista: 'Pulsa "Cargar" hasta ✓ estable, luego "Descargar". Mira cómo baja la curva roja de Q(t) y pulsa Comprobar.',
    explica: 'Al descargar, la carga baja como Q = Q₀·e^(−t/RC): en 3τ queda solo el 5 %.',
    objetivo: g => {
      let qmax = 0;
      for (const h of g.hist) if (h.m === 'carga' && h.q > qmax) qmax = h.q;
      if (qmax < 0.95 * g.P.Vs * g.P.C) return [false, 'Primero carga el capacitor por completo (✓ estable).'];
      if (g.mode !== 'descarga') return [false, 'Ahora pulsa "Descargar".'];
      return g.Q < 0.05 * qmax ? [true]
        : [false, `Todavía queda ${Math.round(100 * g.Q / qmax)} % de la carga. Espera un poco más.`];
    },
  },
  {
    titulo: 'Jefe final',
    meta: 'τ = 2 s y Q = 10 mC',
    historia: '¡El gran reto! Solo hay pilas de 5 V, resistores de 1 kΩ y capacitores de 1000 µF. Arma un circuito con τ = 2 s y que guarde 10 mC.',
    inicial: { bat: [[5]], res: [[1000]], cap: [[1000]] },
    editar: [], agregar: ['bat', 'res', 'cap'], nuevo: { bat: 5, res: 1000, cap: 1000 },
    pista: 'Q = C·V y τ = R·C. Una opción: dos capacitores en paralelo (2 mF) con una pila de 5 V… ¿y qué R da τ = 2 s? Hay más de una solución.',
    explica: 'Combinando serie y paralelo se pueden lograr muchos valores con pocas piezas distintas.',
    objetivo: g => {
      const tOk = cerca(g.P.tau, 2, 0.05), qOk = cerca(g.Q, 0.01, 0.05);
      if (tOk && qOk) return [true];
      const m = [];
      if (!tOk) m.push(`τ = ${si(g.P.tau, 's')} (debe ser 2 s)`);
      if (!qOk) m.push(`Q = ${si(g.Q, 'C')} (debe ser 10 mC, cargado por completo)`);
      return [false, m.join(' y ') + '.'];
    },
  },
];