(function () {
  'use strict';
  let initialized = false;
  let config, columns = [], applied = [], templates = [];
  const storageKey = 'sportgen.table.v2';
  const templateKey = 'sportgen.templates.v2';
  const $id = id => document.getElementById(id);
  const array = value => Array.isArray(value) ? value : value == null ? [] : [value];
  const labels = () => config.labels;
  const valid = value => [...new Set(array(value).filter(x => typeof x === 'string' && Object.hasOwn(labels(), x)))];
  const equal = (a, b) => JSON.stringify(a) === JSON.stringify(b);
  function announce(text) { $id('column-draft-status').textContent = text; }
  function readStorage(key, fallback) {
    try { return JSON.parse(localStorage.getItem(key)) || fallback; } catch (_) { return fallback; }
  }
  function writeStorage(key, value) {
    try { localStorage.setItem(key, JSON.stringify(value)); return true; }
    catch (_) { announce('Браузер не разрешил сохранение. Скачайте шаблон JSON.'); return false; }
  }
  function icon(name) {
    const shapes = {
      up: '<path d="m6 15 6-6 6 6"/>', down: '<path d="m6 9 6 6 6-6"/>',
      remove: '<path d="m6 6 12 12M6 18 18 6"/>',
      grip: '<circle cx="8" cy="5" r="1"/><circle cx="16" cy="5" r="1"/><circle cx="8" cy="12" r="1"/><circle cx="16" cy="12" r="1"/><circle cx="8" cy="19" r="1"/><circle cx="16" cy="19" r="1"/>'
    };
    return '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.7" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">' + shapes[name] + '</svg>';
  }
  function move(field, offset) {
    const from = columns.indexOf(field), to = from + offset;
    if (from < 0 || to < 0 || to >= columns.length) return;
    columns.splice(to, 0, columns.splice(from, 1)[0]);
    render();
    const target = document.querySelector('[data-action-field="' + field + '"][data-action="' + (offset < 0 ? 'up' : 'down') + '"]');
    if (target && !target.disabled) target.focus();
  }
  function render() {
    document.querySelectorAll('[data-column]').forEach(input => { input.checked = columns.includes(input.dataset.column); });
    document.querySelectorAll('.column-group').forEach(group => {
      const fields = array(config.groups[Object.keys(config.groups)[Number(group.dataset.group) - 1]]);
      const count = fields.filter(x => columns.includes(x)).length;
      group.querySelector('.group-count').textContent = count + ' / ' + fields.length;
      const toggle = group.querySelector('[data-group-toggle]');
      toggle.checked = count === fields.length;
      toggle.indeterminate = count > 0 && count < fields.length;
    });
    $id('selected-count').textContent = 'Выбрано колонок: ' + columns.length;
    $id('empty-selection').hidden = columns.length > 0;
    $id('apply-columns').disabled = columns.length === 0;
    const list = $id('selected-column-list');
    list.replaceChildren();
    columns.forEach((field, i) => {
      const row = document.createElement('li');
      row.className = 'selected-row'; row.dataset.selected = field; row.draggable = true;
      const handle = document.createElement('span'); handle.className = 'drag-handle'; handle.innerHTML = icon('grip');
      const number = document.createElement('span'); number.className = 'selected-number'; number.textContent = i + 1;
      const label = document.createElement('span'); label.className = 'selected-label'; label.textContent = labels()[field];
      row.append(handle, number, label);
      [['up', 'Выше: '], ['down', 'Ниже: '], ['remove', 'Убрать: ']].forEach(([action, title]) => {
        const button = document.createElement('button'); button.type = 'button'; button.className = 'row-action';
        button.dataset.action = action; button.dataset.actionField = field;
        button.title = title + labels()[field]; button.setAttribute('aria-label', button.title); button.innerHTML = icon(action);
        button.disabled = (action === 'up' && i === 0) || (action === 'down' && i === columns.length - 1);
        button.addEventListener('click', () => {
          if (action === 'remove') { columns = columns.filter(x => x !== field); render(); }
          else move(field, action === 'up' ? -1 : 1);
        });
        row.append(button);
      });
      row.addEventListener('dragstart', event => { event.dataTransfer.setData('text/plain', field); event.dataTransfer.effectAllowed = 'move'; row.classList.add('dragging'); });
      row.addEventListener('dragend', () => row.classList.remove('dragging'));
      row.addEventListener('dragover', event => { event.preventDefault(); row.classList.add('drop-target'); });
      row.addEventListener('dragleave', () => row.classList.remove('drop-target'));
      row.addEventListener('drop', event => {
        event.preventDefault();
        const source = event.dataTransfer.getData('text/plain');
        const from = columns.indexOf(source), to = columns.indexOf(field);
        if (from >= 0 && to >= 0) { columns.splice(to, 0, columns.splice(from, 1)[0]); render(); }
      });
      list.append(row);
    });
    document.querySelectorAll('[data-preset]').forEach(button => {
      const active = equal(columns, array(config.presets[button.dataset.preset]));
      button.classList.toggle('active', active); button.setAttribute('aria-pressed', String(active));
    });
    announce(!columns.length ? 'Выберите хотя бы одну колонку.' : equal(columns, applied) ? 'Набор применён к таблице.' : 'Есть изменения. Нажмите «Применить к таблице».');
    Shiny.setInputValue('column_draft', columns, {priority:'event'});
  }
  function apply(navigate) {
    if (!columns.length) return;
    applied = columns.slice();
    Shiny.setInputValue('table_columns', applied, {priority:'event'});
    writeStorage(storageKey, {columns:applied});
    render();
    if (navigate) Shiny.setInputValue('show_results', Date.now(), {priority:'event'});
  }
  function filterColumns() {
    const query = $id('column-search').value.trim().toLocaleLowerCase('ru');
    let shown = 0;
    document.querySelectorAll('.column-group').forEach(group => {
      let count = 0;
      group.querySelectorAll('.column-option').forEach(option => {
        const field = option.dataset.field;
        const aliases = {mirna:'микро рнк microRNA miRNA', lncrna:'lncRNA длинная некодирующая РНК', circrna:'circRNA кольцевая РНК', cell_line:'клетки клеточная линия cell line', snp:'snp smp снип'};
        const text = labels()[field] + ' ' + field + ' ' + (aliases[field] || '');
        option.hidden = !text.toLocaleLowerCase('ru').includes(query);
        if (!option.hidden) count++;
      });
      group.hidden = count === 0; shown += count;
      if (query && count) {
        if (!group.hasAttribute('data-before-search')) group.dataset.beforeSearch = String(group.open);
        group.open = true;
      } else if (!query && group.hasAttribute('data-before-search')) {
        group.open = group.dataset.beforeSearch === 'true'; delete group.dataset.beforeSearch;
      }
    });
    $id('column-search-empty').hidden = shown > 0;
  }
  function refreshTemplates() {
    const select = $id('template-list'); select.replaceChildren(new Option('Выберите шаблон', ''));
    templates.forEach((template, i) => select.add(new Option(template.name, String(i))));
  }
  function initialize() {
    if (initialized || !$id('column-config') || !window.Shiny) return;
    initialized = true; config = JSON.parse($id('column-config').textContent);
    Object.keys(config.labels).forEach(key => { config.labels[key] = array(config.labels[key]).join(''); });
    const remembered = readStorage(storageKey, {});
    columns = valid(remembered.columns); if (!columns.length) columns = array(config.initial);
    applied = columns.slice();
    templates = array(readStorage(templateKey, [])).filter(x => x && typeof x.name === 'string' && valid(x.columns).length).slice(0, 30).map(x => ({name:x.name.slice(0,64), columns:valid(x.columns)}));
    refreshTemplates(); render(); Shiny.setInputValue('table_columns', applied);
    document.querySelectorAll('[data-column]').forEach(input => input.addEventListener('change', () => {
      const field = input.dataset.column;
      columns = input.checked ? valid([...columns, field]) : columns.filter(x => x !== field); render();
    }));
    document.querySelectorAll('[data-group-toggle]').forEach(input => input.addEventListener('change', () => {
      const fields = array(Object.values(config.groups)[Number(input.dataset.groupToggle) - 1]);
      columns = input.checked ? valid([...columns, ...fields]) : columns.filter(x => !fields.includes(x)); render();
    }));
    document.querySelectorAll('[data-preset]').forEach(button => button.addEventListener('click', () => {
      columns = array(config.presets[button.dataset.preset]).slice(); render();
    }));
    $id('select-all-columns').addEventListener('click', () => { columns = Object.keys(labels()); render(); });
    $id('clear-columns').addEventListener('click', () => { columns = []; render(); });
    $id('reset-column-draft').addEventListener('click', () => { columns = applied.slice(); render(); });
    $id('apply-columns').addEventListener('click', () => apply(true));
    $id('column-search').addEventListener('input', filterColumns);
    ['collapse','expand'].forEach(action => $id(action + '-columns').addEventListener('click', () => {
      document.querySelectorAll('.column-group').forEach(group => { group.open = action === 'expand'; });
    }));
    $id('template-save').addEventListener('click', () => {
      const name = $id('template-name').value.trim().slice(0,64);
      if (!name || !columns.length) { announce('Введите название и выберите хотя бы одну колонку.'); return; }
      const existing = templates.findIndex(x => x.name === name);
      if (existing < 0 && templates.length >= 30) { announce('Сохранено 30 шаблонов. Удалите ненужный или скачайте JSON.'); return; }
      const template = {name, columns:columns.slice()};
      if (existing < 0) templates.push(template); else templates[existing] = template;
      refreshTemplates();
      if (writeStorage(templateKey, templates)) announce('Шаблон «' + name + '» сохранён в этом браузере.');
    });
    $id('template-load').addEventListener('click', () => {
      const value = $id('template-list').value; if (value === '') { announce('Выберите сохранённый шаблон.'); return; }
      columns = valid(templates[Number(value)].columns); render();
    });
    $id('template-delete').addEventListener('click', () => {
      const value = $id('template-list').value; if (value === '') return;
      templates.splice(Number(value),1); refreshTemplates(); writeStorage(templateKey, templates); announce('Шаблон удалён. Текущие колонки сохранены.');
    });
    $id('template-download').addEventListener('click', () => {
      if (!columns.length) { announce('Сначала выберите колонки.'); return; }
      const value = {schema_version:1, name:$id('template-name').value.trim().slice(0,64) || 'Мой набор', columns:columns.slice()};
      const url = URL.createObjectURL(new Blob([JSON.stringify(value,null,2)], {type:'application/json'}));
      const link = document.createElement('a'); link.href = url; link.download = 'sportgen-table-template.json'; link.click();
      setTimeout(() => URL.revokeObjectURL(url), 1000);
    });
    $id('template-import').addEventListener('change', async event => {
      const file = event.target.files[0]; if (!file) return;
      try {
        if (file.size > 100000) throw new Error();
        const obj = JSON.parse(await file.text());
        if (obj.schema_version !== 1 || !Array.isArray(obj.columns) || !valid(obj.columns).length) throw new Error();
        columns = valid(obj.columns); $id('template-name').value = typeof obj.name === 'string' ? obj.name.slice(0,64) : 'Импортированный набор';
        render(); announce('Шаблон открыт. Примените набор или сохраните его в браузере.');
      } catch (_) { announce('Не удалось открыть шаблон. Выберите JSON, скачанный кнопкой «Скачать JSON».'); }
      event.target.value = '';
    });
    Shiny.addCustomMessageHandler('column-preset', message => {
      columns = valid(message.columns); if (!columns.length) columns = array(config.initial);
      if (message.apply) apply(false); else render();
    });
    Shiny.addCustomMessageHandler('search-running', state => {
      const button = $id('run'); button.disabled = state.running;
      button.textContent = state.running ? 'Поиск выполняется…' : 'Запустить поиск';
      if (!state.running && ['success','warning'].includes(state.kind)) { $id('search-panel').open = false; adjustTables(); }
    });
    $id('run').addEventListener('click', () => { $id('run').disabled = true; $id('run').textContent = 'Поиск выполняется…'; });
    $id('toggle-column-filters').addEventListener('click', event => {
      const active = document.body.classList.toggle('show-column-filters'); event.currentTarget.setAttribute('aria-pressed', String(active)); adjustTables();
    });
    $id('toggle-table-density').addEventListener('click', event => {
      const active = document.body.classList.toggle('compact-table'); event.currentTarget.setAttribute('aria-pressed', String(active));
      event.currentTarget.textContent = active ? 'Обычные строки' : 'Компактные строки'; adjustTables();
    });
    $id('expand-table').addEventListener('click', () => setExpanded(!document.body.classList.contains('table-expanded')));
    $id('clear-table-filters').addEventListener('click', () => {
      if (!window.jQuery || !jQuery.fn.dataTable) return;
      const table = jQuery('#results .dataTables_scrollBody table').first();
      if (jQuery.fn.dataTable.isDataTable(table)) {
        table.DataTable().search('').columns().search('').draw();
        jQuery('#results .dataTables_filter input').val('');
        jQuery('#results .dataTables_scrollHead thead input').val('');
      }
    });
    document.addEventListener('keydown', event => { if (event.key === 'Escape') setExpanded(false); });
    document.addEventListener('toggle', event => { if (event.target instanceof HTMLDetailsElement) adjustTables(); }, true);
    document.addEventListener('click', event => {
      const button = event.target.closest('.cell-expand-button'); if (!button) return;
      const cell = button.closest('.expandable-cell'), expanded = button.getAttribute('aria-expanded') === 'true';
      button.setAttribute('aria-expanded', String(!expanded)); button.textContent = expanded ? 'Показать полностью' : 'Свернуть';
      cell.querySelector('.cell-preview').hidden = !expanded; cell.querySelector('.cell-full').hidden = expanded;
    });
    jQuery(document).on('shown.bs.tab', adjustTables);
    window.addEventListener('resize', adjustTables);
  }
  function adjustTables() {
    requestAnimationFrame(() => {
      if (window.jQuery && jQuery.fn.dataTable) jQuery.fn.dataTable.tables({visible:true, api:true}).columns.adjust();
    });
  }
  function setExpanded(active) {
    document.body.classList.toggle('table-expanded', active);
    const button = $id('expand-table'); button.setAttribute('aria-pressed', String(active));
    button.textContent = active ? 'Вернуть размер · Esc' : 'На весь экран'; adjustTables();
  }
  if (window.jQuery) jQuery(document).on('shiny:connected', initialize);
  document.addEventListener('DOMContentLoaded', () => {
    if (window.jQuery) jQuery(document).on('shiny:connected', initialize);
    if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && Shiny.shinyapp.$socket.readyState === 1) initialize();
  });
}());
